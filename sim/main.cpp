#include "VCore.h"

#include <verilated.h>
#include <verilated_vcd_c.h>

#include <memory>
#include <vector>
#include <iostream>
#include <fstream>
#include <unordered_map>
#include <queue>

#include <cstdint>
#include <cassert>

#include <sys/select.h>
#include <sys/time.h>

using Word = std::uint32_t;

const double TIMESCALE       = 1e-9;
const int    CLOCK_FREQUENCY = 100*1e6;
const int    CLOCK_PERIOD    = 1/(CLOCK_FREQUENCY*TIMESCALE);

const std::uint64_t MAX_CYCLES = 1000000000ULL;

const std::size_t DL1_CACHE_LINE_MASK = 0x0f;
const std::size_t DL1_CACHE_LINE_SIZE = 16;  // bytes
const std::size_t DL1_SIZE            = 64;  // bytes

const vluint64_t  AXI_LATENCY  = 1; // cycles
const vluint64_t  DL1_LATENCY  = 0; // cycles
const vluint64_t  DMEM_LATENCY = 1; // cycles
const vluint64_t  IMEM_LATENCY = 0; // cycles

struct ReadWord
{
  Word nextReadWord_;
  vluint64_t nextReadCycle_;
  vluint8_t nextReadId_;

#if 0
  // Required for min priority queue
  friend bool operator< (ReadWord const& lhs, ReadWord const& rhs) {
    return lhs.nextReadCycle_ > rhs.nextReadCycle_;
  }
#endif
};

class ReadQueue
{
public:
  ReadQueue(int id) : id_(id) {}

  bool empty(vluint64_t cycle) const
  {
    if (queue_.empty())
      return true;

    return queue_.front().nextReadCycle_ > cycle;
  }

  const ReadWord & front() const
  {
    return queue_.front();
  }

  void pop()
  {
    queue_.pop();
  }

  void push(const ReadWord &rw)
  {
    queue_.push(rw);
  }

private:
  int id_;
  std::queue<ReadWord> queue_; // TODO: Use queue per id
};

class Memory
{
public:

    Memory(VCore& top, const char* memoryFile) :
      top_{top}, readCodeQ_(0), readDataQ_(8)
    {
        auto ifs = std::ifstream{memoryFile, std::ifstream::binary};
        auto memoryBytes =
            std::vector<unsigned char>{std::istreambuf_iterator<char>(ifs), {}};

        assert((memoryBytes.size() % 4 == 0) &&
               "Memory does not contain a multiple of words");

        auto i = std::size_t{0};

        while (i < memoryBytes.size())
        {
            auto b0 = memoryBytes[i++];
            auto b1 = memoryBytes[i++];
            auto b2 = memoryBytes[i++];
            auto b3 = memoryBytes[i++];

            auto word = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
            memory_.push_back(word);
        }

        codeSize_ = memoryBytes.size();
    }

    void eval(vluint64_t cycle)
    {
        top_.io_axi_arw_ready = true;
        top_.io_axi_w_ready = true;
        top_.io_axi_r_valid = false;
        top_.io_axi_b_valid = false;

        if (! readDataQ_.empty(cycle))
        {
          putOnBus(readDataQ_.front());
          readDataQ_.pop();
        }
        else if (! readCodeQ_.empty(cycle))
        {
          putOnBus(readCodeQ_.front());
          readCodeQ_.pop();
        }

        if (top_.io_axi_arw_valid)
        {
            if (top_.io_axi_arw_payload_write)
            {
                write(top_.io_axi_arw_payload_addr,
                      top_.io_axi_w_payload_strb,
                      top_.io_axi_w_payload_data);

                top_.io_axi_b_valid = true;
            }
            else
            {
                auto address = top_.io_axi_arw_payload_addr;
                auto latency = getLatency(address);

                assert(latency > 0 && "Invalid latency");

#if 0
                std::cout << "ADDR=" << std::hex << address;
                std::cout << " TYPE=";
                std::cout << (isDataAddress(address) ? "DATA" : "CODE");
                std::cout << " LATENCY=" << std::dec << latency;
                std::cout << std::endl;
#endif

                ReadWord rw;

                rw.nextReadWord_ = read(address);
                rw.nextReadCycle_ = cycle + latency;
                rw.nextReadId_ = top_.io_axi_arw_payload_id;

                if (isDataAddress(address))
                  readDataQ_.push(rw);
                else
                  readCodeQ_.push(rw);
            }
        }
    }

    void dump(const char* fname)
    {
      std::fstream fs;
      fs.open (fname, std::fstream::out | std::fstream::binary);

      for(const auto& word: memory_)
      {
        const char *bytes = reinterpret_cast<const char *>(&word);
        fs.write(bytes, sizeof(word));
      }

      fs.close();
    }

private:

    using Address = std::uint32_t;
    using Mask = std::uint8_t;

    bool isDataAddress(Address address)
    {
        return ((address >> 2) > codeSize_);
    }

    Address dL1Index(Address address)
    {
        return address % DL1_SIZE;
    }

    void addToCache(Address address)
    {
        if (isDataAddress(address))
        {
            auto index = dL1Index(address);

#if 0
                std::cout << "CACHE " << std::hex << address;
                std::cout << " @ " << std::dec << index << std::endl;
#endif

            dL1_[index] = address;
        }
    }

    void putOnBus(const ReadWord &rw)
    {
#if 0
      std::cout << "DATA=" << std::hex << rw.nextReadWord_ << std::endl;
#endif

      top_.io_axi_r_payload_data = rw.nextReadWord_;
      top_.io_axi_r_payload_id = rw.nextReadId_;
      top_.io_axi_r_payload_last = true;
      top_.io_axi_r_valid = true;
      //rw.nextReadCycle_ = 0;

      assert(top_.io_axi_r_ready);
    }

    Word read(Address address)
    {
        ensureEnoughMemory(address);

        Address addr = address & ~(DL1_CACHE_LINE_MASK);
        for (int i=0; i<DL1_CACHE_LINE_SIZE; i++)
          addToCache(addr+i);

        return memory_[(address >> 2)];
    }

    void write(Address address, Mask mask, Word value)
    {
        ensureEnoughMemory(address);

        auto bitMask = Word{0};
        if (mask & 0x1) bitMask |= 0x000000ff;
        if (mask & 0x2) bitMask |= 0x0000ff00;
        if (mask & 0x4) bitMask |= 0x00ff0000;
        if (mask & 0x8) bitMask |= 0xff000000;

        auto& memoryValue = memory_[(address >> 2)];
        memoryValue &= ~bitMask;
        memoryValue |= value & bitMask;

        addToCache(address);
    }

    void ensureEnoughMemory(Address address)
    {
        if ((address >> 2) >= memory_.size())
        {
            memory_.reserve((address >> 2) + 1);

            while ((address >> 2) >= memory_.size())
                memory_.push_back(0xcafebabe);
        }
    }

    bool isDL1Cached(Address address)
    {
        if (!isDataAddress(address))
            return false;

        auto index = dL1Index(address);
        auto entry = dL1_.find(index);

        if (entry == dL1_.end())
            return false;

        return entry->second == address; 
    }

    vluint64_t getLatency(Address address)
    {
        vluint64_t extraDelay = 0;

        if (isDataAddress(address))
        {
            extraDelay = DMEM_LATENCY;
            if (isDL1Cached(address))
                extraDelay = DL1_LATENCY;
        }
        else 
        {
            extraDelay = IMEM_LATENCY;
        }

        return AXI_LATENCY + extraDelay;
    }

    VCore& top_;
    std::vector<Word> memory_;
    std::size_t codeSize_;
    std::unordered_map<Address, Address> dL1_;
    ReadQueue readCodeQ_;
    ReadQueue readDataQ_;
};

class CharDev
{
public:

    CharDev(VCore& top) : top_{top}, gotEot_{false}
    {
    }

    void eval()
    {
        if (top_.io_charOut_valid)
        {
            auto charOut = char(top_.io_charOut_payload);

            if (charOut == 0x4)
                gotEot_ = true;
            else
            {
                gotEot_ = false;
                std::cout << charOut;
            }
        }
    }

    bool gotEot() const
    {
        return gotEot_;
    }

private:

    VCore& top_;
    bool gotEot_;
};

class TestDev
{
public:

    TestDev(VCore& top) : top_{top}, result_{-1}
    {
    }

    void eval()
    {
        if (top_.io_testDev_valid)
            result_ = top_.io_testDev_payload;
    }

    bool gotResult() const
    {
        return result_ >= 0;
    }

    bool hasFailed() const
    {
        return gotResult() && result_ != 0;
    }

    int failedTest() const
    {
        assert(hasFailed() && "No failed tests");
        return result_;
    }

private:

    VCore& top_;
    int result_;
};

class ByteDev
{
public:

    ByteDev(VCore& top) : top_{top}
    {
    }

    bool eval()
    {
        if (top_.reset)
            return false;

        top_.io_byteDev_rdata_valid = false;

        if (top_.io_byteDev_wdata_valid)
        {
            auto charOut = char(top_.io_byteDev_wdata_payload);
            std::cout << charOut;
        }

        if (!hasStdinByte && stdinAvailable())
        {
            currentStdinByte = std::cin.get();
            hasStdinByte = !std::cin.eof();
        }

        if (hasStdinByte)
        {
            top_.io_byteDev_rdata_valid = true;
            top_.io_byteDev_rdata_payload = currentStdinByte;

            if (top_.io_byteDev_rdata_ready)
                hasStdinByte = false;

            return true;
        }

        return false;
    }

private:

    bool stdinAvailable() const
    {
        if (std::cin.eof())
            return false;

        fd_set rfds;
        FD_ZERO(&rfds);
        FD_SET(STDIN_FILENO, &rfds);

        timeval tv;
        tv.tv_sec = 0;
        tv.tv_usec = 0;

        int result = select(1, &rfds, nullptr, nullptr, &tv);
        return result == 1;
    }

    VCore& top_;
    char currentStdinByte;
    bool hasStdinByte = false;
};

int main(int argc, char** argv)
{
    assert(argc >= 2 && "No memory file name given");

    Verilated::commandArgs(argc, argv);

    auto top = std::unique_ptr<VCore>{new VCore};
    top->reset = 1;
    top->clk = 1;

    auto memoryFile = argv[argc - 1];
    auto memory = Memory{*top, memoryFile};
    auto charDev = CharDev{*top};
    auto testDev = TestDev{*top};
    auto byteDev = ByteDev{*top};

    Verilated::traceEverOn(true);
    auto tracer = std::unique_ptr<VerilatedVcdC>{new VerilatedVcdC};
    top->trace(tracer.get(), 99);
    tracer->open("sim.vcd");

    vluint64_t mainTime = 0;
    vluint64_t cycle = 0;
    auto isDone = false;
    int result = 0;

    while (!isDone)
    {
        auto clockEdge = (mainTime % (CLOCK_PERIOD/2) == 0);

        if (clockEdge)
            top->clk = !top->clk;

        if (mainTime >= 5*CLOCK_PERIOD)
            top->reset = 0;

        top->eval();

        if (clockEdge && top->clk)
        {
            cycle++;

            memory.eval(cycle);
            top->eval();
            //memory.eval(cycle);
            //top->eval();

            charDev.eval();
            testDev.eval();

            if (charDev.gotEot())
                isDone = true;

            if (testDev.gotResult())
            {
                isDone = true;

                if (testDev.hasFailed())
                {
                    std::cerr << "Test " << testDev.failedTest() << " failed\n";
                    result = 1;
                }
                else
                    std::cout << "All tests passed\n";
            }

            if (byteDev.eval())
                top->eval();

            if (mainTime >= MAX_CYCLES*CLOCK_PERIOD)
            {
                isDone = true;
                result = 1;
            }
        }

        tracer->dump(mainTime);

        mainTime++;
    }

    tracer->close();
    memory.dump("sim.mem");
    return result;
}
