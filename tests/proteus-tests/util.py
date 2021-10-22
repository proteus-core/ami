from types import SimpleNamespace
import subprocess
import tempfile

###########################################################################
def disassemble(bytez):

  assert len(bytez) == 4

  with tempfile.NamedTemporaryFile() as f:
    f.write(bytez)
    f.flush()

    objdump  = "riscv64-unknown-elf-objdump"
    objdump += " -D"
    objdump += " -b binary"
    objdump += " -m riscv"
    objdump += " -M no-aliases"
    objdump += " %s" % f.name

    proc = subprocess.run(objdump.split(),
                          stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE,
                          universal_newlines=True)

    if proc.returncode == 0:
      for line in proc.stdout.split('\n'):
        parts = line.split()
        if len(parts) >= 3 and parts[0] == '0:':
          return ' '.join(parts[2:])

    return bytez.tostring()
 
#############################################################################
class NestedNamespace(SimpleNamespace):
  def __init__(self, dictionary, **kwargs):
    super().__init__(**kwargs)
    for key, value in dictionary.items():
      if isinstance(value, dict):
        self.__setattr__(key, NestedNamespace(value))
      else:
        self.__setattr__(key, value)
