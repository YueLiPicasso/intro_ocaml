# OCaml and OCanren Study Notes

This repo hosts artifects created while I was learning the OCaml language and
subsequently when I was learning [OCanren](https://github.com/JetBrains-Research/OCanren).

## OCanren Examples

The [OCanren_exericese](OCanren_exericese) directory contains several interesting (recreational)
mathematical problems solved using OCanren, such as planning for a jeep the strategy to
cross a desert, or for a aircraft fleet the way to achieve a ccertain range, or even for a group
robbers the way to evenly share their booty !


## Other Goodies

The [sessions](sessions) folder are exercise code that I wrote following several OCaml resources.

[Hanoi](Hanoi) solves the Tower of Hanoi problem in OCaml and SWI-Prolog. Translating the code into OCanren  is an ongoing exercise.



## Compiling OCanren Source

An OCanren program is stored in a `.ml` file as normal OCaml sources, and is run not with the OCaml toplevel but with the compiled and linked executable.
There is certain complication involved in the compilation process, but this is simplified  by using a standard
[makefile](https://github.com/YueLiPicasso/intro_ocaml/blob/master/Hanoi/ocanren/Makefile). To learn how to read and write a makefile, see the 
book [_GNU make_](https://www.gnu.org/software/make/manual/).

In the simplest case, the OCanren programmer only needs two files in the working directory (after
installation of OCanren system-wide): a `.ml` source file and a copy of the standard makefile.

Some customization is required for the standard makefile so that it could work
with the particular source file. For instance, given the source file `<source-name>.ml` the two variables TOPFILE and SOURCES (appearing at the top of the makefile) shall be redefined respectively as

> `TOPFILE = <source-name>`

> `SOURCES = <source-name>.ml`

Then, to compile and link the OCanren source `<source-name>.ml` one shall simply run the shell command `make` in the source directory, followed by `./<source-name>.opt` to run the executable. 

Note that a native code executable has the extension `.native ` or `.opt` by default, and a byte code executable has the extension `.byte`. The user can use other extensions, only to remember to update the relevant parts the makefile accordingly! 

## Installation Notes

### Installing OCanren

Basically, following the instructions given on the OCanren [repo](https://github.com/JetBrains-Research/OCanren). To make OCanren available system-wide, further run the command `make install` from the OCanren source directory.

Avoid using `sudo`. Mixing `make` and `sudo make` during installation may cause problems with locating packages:
if any such problem occurs, run `ls -l` under the OCanren directory to see, e.g., if the `_build` folder is created as root, and if so remove it `sudo rm -Rf _build` then `make clean` then `make` followed by `make install`.



### Installing OCaml

I set up OCaml following the instructions from [Real World Ocaml](http://dev.realworldocaml.org/install.html). OCaml is very Windows-unfriendly, so I had to learn how to set up a Linux operating
system first.  

### Ubuntu Linux Live USB Setup

I created a bootable USB to run Ubuntu Linux without installation on my (otherwise Windows) PC following the Ubuntu [toturial](https://ubuntu.com/tutorials/tutorial-create-a-usb-stick-on-ubuntu#1-overview). A persistent storage (> 100 GB) was created with the help from [UUI](https://www.pendrivelinux.com/universal-usb-installer-easy-as-1-2-3) and a set of online tutorials (see below). The trick is to create a very small (10 MB) casper-rw file while installing Ubuntu on the USB, and then use Ubuntu's GParted partition manager to create a large (> 100 GB) casper-rw partition on the USB drive (_but not to delete the casper-rw file after creating the casper-rw partition_). This solution to persistent storage is somewhat both a mixture of and a deviation from the tutorials on [Stack Exchange](https://askubuntu.com/questions/397481/how-to-make-a-persistent-live-ubuntu-usb-with-more-than-4gb) and [How-to Geek](https://www.howtogeek.com/howto/14912/create-a-persistent-bootable-ubuntu-usb-flash-drive/). The details are as follows:

* First I got two USB drives, one 300 GB, the other 4 GB.
* I then, on my Windows PC, downloaded and installed the lastest Ubuntu ISO image on both USB drives, following the Ubuntu [toturial](https://ubuntu.com/tutorials/tutorial-create-a-usb-stick-on-ubuntu#1-overview). This tutorial uses Rufus but I used UUI instead since somehow I had problem downloading Rufus. It was important that _a 10 MB casper-rw file was created when installing Ubuntu on the 300 GB USB drive_.
* Then, the challenge was to open the [UEFI](https://www.windowscentral.com/how-enter-uefi-bios-windows-10-pcs) settings page while restarting the PC, with the 4 GB USB inserted and the 300 GB USB detached. I use an Acer laptop and the way that works for me is to quickly press the F2 key several times when the computer is just starting, i.e. when it is showing an Acer logo on the screen before the Microsoft Windows splash screen is shown. 
* From the UEFI settings page, I changed the boot mode from UEFI to Legacy, and increased the priority of the option of booting from USB. Switching bewteen the UEFI mode and the Legacy mode is needed to switch between Windows and Linux.
* After successfully launched Ubuntu from the 4 GB USB, I inserted the 300 GB USB, and made the EXT4 casper-rw partition on it.
* Then I restarted the computer to launch Ubuntu from the 300 GB USB.

The above sequence of actions is just a success trace. I had many failed attempts before this one was found to work. With a persistent storage all files downloaded during a Ubuntu live USB session would be stored and still be available at the next session.  
