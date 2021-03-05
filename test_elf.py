#!/usr/bin/env python3

import os
import stat
import sys

#
# What we need to make a dynamically linked ELF file:
# - ELF header.
# - Three program segments: one with the name of the interpreter, one with
#   executable instructions, and one with dynamic linking information.
# - String table (will contain the interpreter's path, library names, and
#   dynamically linked symbol names).
# - Symbol table.
# - Relocation table.
#

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
        b'\x03\x00'
        # Size of a section header table entry.
        b'\x00\x00'
        # Number of entries in the section header table.
        b'\x00\x00'
        # Index of the section header table entry that contains the section
        # names.
        b'\x00\x00')

# Make 64-bit program header. Note that 32-bit program headers have different
# member order.
def make_program_header(segment_type, offset, size, align):
    segment_type = ['load', 'dynamic', 'interpreter'].index(segment_type) + 1
    return (
        # Segment type.
        segment_type.to_bytes(4, 'little') +
        # Flags.
        b'\x07\x00\x00\x00' # Read, write, and execute.
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
        + (align).to_bytes(8, 'little'))

string_table = (
    # First byte must be null.
    b'\x00'
    b'/lib64/ld-linux-x86-64.so.2\x00'
    b'libc.so.6\x00'
    b'puts\x00'
    # XXX: hack to guarantee alignment (see assert below).
    b'\x00\x00\x00\x00')

symbol_table = (
    # Undefined symbol.
    b'\x00\x00\x00\x00\x00\x00\x00\x00'
    b'\x00\x00\x00\x00\x00\x00\x00\x00'
    b'\x00\x00\x00\x00\x00\x00\x00\x00'
    # `puts`
    # Symbol name offset (on string table).
    + (39).to_bytes(4, 'little') +
    # Global, untyped symbol
    # (see `https://refspecs.linuxfoundation.org/elf/gabi4+/ch4.symtab.html).
    b'\x10\x00\x00\x00'
    # Symbol value.
    b'\x00\x00\x00\x00\x00\x00\x00\x00'
    # Symbol size.
    b'\x00\x00\x00\x00\x00\x00\x00\x00')

relocation_table = (
    # Virtual address to apply the relocation at.
    b'\x09\x10\x40\x00\x00\x00\x00\x00'
    # Relocation type.
    b'\x01\x00\x00\x00'
    # Symbol table index.
    b'\x01\x00\x00\x00'
    # Addend used to compute the value to be stored into the relocatable field.
    b'\x00\x00\x00\x00\x00\x00\x00\x00')

# In ELF lingo, the segments contain information needed at runtime, while the
# sections contain information needed during linking.
dynamic_section = (
    # Needed library (`libc.so.6`) name at offset 29 on string table.
    (1).to_bytes(8, 'little') + (29).to_bytes(8, 'little') +
    # Virtual address of string table.
    (5).to_bytes(8, 'little') +
    (0x400000 + 232).to_bytes(8, 'little') +
    # Symbol table address.
    (6).to_bytes(8, 'little') +
    (0x400000 + 232 + len(string_table)).to_bytes(8, 'little') +
    # Relocation table address.
    (7).to_bytes(8, 'little') +
    (0x400000 + 232 + len(string_table) + len(symbol_table)).to_bytes(8, 'little') +
    # Size (in bytes) of relocation table.
    (8).to_bytes(8, 'little') +
    (len(relocation_table)).to_bytes(8, 'little') +
    # Size (in bytes) of relocation table entries.
    (9).to_bytes(8, 'little') + (24).to_bytes(8, 'little') +
    (0).to_bytes(8, 'little'))

payload = (
    # `lea msg(%eip), %edi`
    b'\x67\x8d\x3d\x15\x00\x00\x00'
    # `mov $puts, %rax`
    b'\x48\xb8\x00\x00\x00\x00\x00\x00\x00\x00'
    # `call *%rax`
    b'\xff\xd0'
    # `mov $60, %eax`
    b'\xb8\x3c\x00\x00\x00'
    # `xor %edi, %edi`
    b'\x31\xff'
    # `syscall`
    b'\x0f\x05'
    # `msg:`
    b'Hi.\x00')

with open('test', 'wb') as f:
    f.write(make_header())
    # Points to the first nonempty string at the string table.
    f.write(make_program_header('interpreter', 233, 28, 1))
    f.write(make_program_header('load', 0,
        4096 + len(payload), 4096))
    f.write(make_program_header('dynamic',
        232 + len(string_table) + len(symbol_table) + len(relocation_table),
        len(dynamic_section), 8))
    f.write(string_table)
    # The next three writes should be aligned to 8.
    assert f.tell() % 8 == 0
    f.write(symbol_table)
    # XXX: relocation table needs to be loaded. That is why the second segment
    # (the `LOAD` segment) loads the entire file.
    f.write(relocation_table)
    f.write(dynamic_section)
    f.seek(4096)
    f.write(payload)

st = os.stat('test')
os.chmod('test', st.st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
