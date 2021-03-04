#!/usr/bin/env python3

import os
import stat
import sys

def make_header():
    return (
        # ELF magic number.
        b'\x7fELF'
        # Executable class, set to `0x01` or `0x02` to signify 32 or 64-bits
        # respectively.
        b'\x02' # 64-bit executable.
        # Data class, set to `0x01` or `0x02` to signify little or big
        # endianness respectively. Affects interpretation of multibyte header
        # fields.
        b'\x01' # Little endian.
        # ELF header version. Always set to `0x01`.
        b'\x01'
        # Target OS ABI.
        b'\x00' # System V.
        # Further ABI specifications.
        b'\x00'
        # Unused.
        b'\x00\x00\x00\x00\x00\x00\x00'
        # Object file type.
        b'\x02\x00' # Executable.
        # Target instruction set architecture.
        b'\x3e\x00' # x86-64.
        # Object file version. Always set to `0x00000001`.
        b'\x01\x00\x00\x00'
        # Virtual memory address of the entry point. This field's size depends
        # on the executable class set.
        b'\x00\x10\x40\x00\x00\x00\x00\x00'
        # Program header table's file offset.
        b'\x40\x00\x00\x00\x00\x00\x00\x00' # Right after ELF header.
        # Section header table's file offset.
        b'\x00\x00\x00\x00\x00\x00\x00\x00' # Absent.
        # Processor-specific flags associated with the file.
        b'\x00\x00\x00\x00'
        # Size of this header.
        b'\x40\x00' # 64 bytes.
        # Size of a program header table entry.
        b'\x38\x00'
        # Number of entries in the program header table.
        b'\x01\x00'
        # Size of a section header table entry.
        b'\x00\x00'
        # Number of entries in the section header table.
        b'\x00\x00'
        # Index of the section header table entry that contains the section
        # names.
        b'\x00\x00')

# Make 64-bit program header. Note that 32-bit program headers have different
# member order.
def make_program_header(offset, size):
    return (
        # Segment type.
        b'\x01\x00\x00\x00' # Loadable segment.
        # Flags (`0x00000005` and `0x00000006` signify read and execute and
        # write and execute respectively).
        b'\x05\x00\x00\x00' # Read and execute.
        # Offset of segment in file.
        + offset.to_bytes(8, 'little')
        # Virtual address of the segment in memory.
        + (offset + 0x400000).to_bytes(8, 'little')
        # Physical address of the segment in memory.
        + (offset + 0x400000).to_bytes(8, 'little')
        # Size of segment in file.
        + size.to_bytes(8, 'little')
        # Size of segment in memory.
        + size.to_bytes(8, 'little')
        # Alignment in file and memory.
        + (4096).to_bytes(8, 'little'))

payload = (
    # `mov $1, %eax`
    b'\xb8\x01\x00\x00\x00'
    # `mov $1, %edi`
    b'\xbf\x01\x00\x00\x00'
    # `mov $msg, %esi`
    b'\xbe\x1f\x10\x40\x00'
    # `mov $4, %edx`
    b'\xba\x04\x00\x00\x00'
    # `syscall`
    b'\x0f\x05'
    # `mov $60, %eax`
    b'\xb8\x3c\x00\x00\x00'
    # `xor %edi, %edi`
    b'\x31\xff'
    # `syscall`
    b'\x0f\x05'
    b'Hi.\n\x00')

with open('test', 'wb') as f:
    f.write(make_header())
    f.write(make_program_header(4096, len(payload)))
    f.seek(4096)
    f.write(payload)

st = os.stat('test')
os.chmod('test', st.st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
