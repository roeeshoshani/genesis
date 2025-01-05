# genesis
a firmware implementation for mips.

## goals
the final goal of this project is to implement a network switch, which switches packets between its
different interfaces.

as for why i am working on this project, this is a hobby project which helps me learn more about
embedded programming.

## what is genesis
genesis is a firmware, which means that it is the first thing that is executed when the device boots up.
as such, it is responsible for configuring all hardware, from cpu caches to pci devices and interrupt controllers.

the current implementation is very specific to the malta board emulated by qemu, as i don't have a physical board
to work with.

## project structure
the project is divided into the following main components:
- loader
- core
- packer

#### loader
the loader is the first thing that is executed.

it is statically compiled, with a known base address, which is the cpu's reset vector address which maps to
flash memory.

the loader is very minimal. it is only responsible for the following operations:
- setting up the stack
- initializing the cpu caches
- loading the kernel's core from flash to ram
- relocating the kernel's core
- and finally, passing control to the kernel's core

basically it sets up a basic environment in which our kernel can run, and runs it.

#### core
the core is the actual kernel, and contains all the actual code.

the core is position independent and is relocated by the loader to run at the address determined at run time.

it is reponsible for the actual "kernel" part of the project.

#### packer
the packer is reponsible for creating the final firmware image from the different parts.

it is implemented as an `xtask` project in the root of the `kernel` directory.

here are some important `xtask` commands which the packer provides:
- `run`: build the firmware and run it using qemu
- `gdb`: connect to the kernel using gdb and load the relevent symbol files

#### libraries
the project contains some more crates which serve as libraries:
- hal: hardware abstraction layer. contains basic definitions about the hardware. shared between the core and the loader since they both do hardware stuff.
- loader_shared: loader code that is shared with other crates.


## state
the project is in a very early state and currently can't do much, but the groundwork for building an actual
functional kernel was laid.

## final note
feel free to follow along the development of this project.

also, feel free to ask me questions, i will be happy to help and i'll do my best to answer them.

if you want to contribute, create a PR, i could use some help...

cheers
