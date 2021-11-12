from types import SimpleNamespace
import subprocess
import tempfile

###########################################################################
def gnu_objdump(f):
  objdump  = "riscv64-unknown-elf-objdump"
  objdump += " -D"
  objdump += " -b binary"
  objdump += " -m riscv"
  objdump += " -M no-aliases"
  objdump += " %s" % f.name

  return subprocess.run(objdump.split(),
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        universal_newlines=True)

###########################################################################
def gnu_objcopy(f1, f2):
  result = tempfile.NamedTemporaryFile()

  objcopy  = "riscv64-unknown-elf-objcopy"
  objcopy += " -I binary"
  objcopy += " -O elf32-littleriscv"
  objcopy += " %s" % f1.name
  objcopy += " %s" % f2.name

  subprocess.run(objcopy.split(),
                 stdout=subprocess.DEVNULL,
                 stderr=subprocess.DEVNULL)

###########################################################################
def llvm_objdump(f):
  with tempfile.NamedTemporaryFile() as f2:
    gnu_objcopy(f, f2)

    objdump  = "/usr/local/morpheus/bin/llvm-objdump"
    objdump += " -D"
    objdump += " %s" % f2.name
  
    return subprocess.run(objdump.split(),
                          stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE,
                          universal_newlines=True)
  
###########################################################################
def disassemble(bytez):

  assert len(bytez) == 4

  with tempfile.NamedTemporaryFile() as f:
    f.write(bytez)
    f.flush()

    proc = llvm_objdump(f)

    if proc.returncode == 0:
      for line in proc.stdout.split('\n'):
        parts = line.split()
        if len(parts) >= 3 and parts[0] == '0:':
          #return ' '.join(parts[2:])
          return ' '.join(parts[5:])

    return bytez.hex()
 
#############################################################################
class NestedNamespace(SimpleNamespace):

  def __init__(self, dictionary, **kwargs):

    super().__init__(**kwargs)

    for key, value in dictionary.items():
      if isinstance(value, dict):
        self.__setattr__(key, NestedNamespace(value))
      else:
        self.__setattr__(key, value)
