# intro_ocaml
Artifects created while learning the OCaml language

[ch1](ch1), ch2, etc. correspond to chapter 1, 2, ... of the OCaml [reference manual](http://caml.inria.fr/pub/docs/manual-ocaml/).

[summer](summer) contains workouts according to a summer school [tutorial](http://caml.inria.fr/pub/docs/u3-ocaml/index.html) by Didier Remy. 

[from_beginning](from_beginning) is created following a [textbook](http://ocaml-book.com/) by John Whitington.

## Installation Notes

### Installing OCaml

I set up OCaml following the instructions from [Real World Ocaml](http://dev.realworldocaml.org/install.html). OCaml is very Windows-unfriendly, so I had to learn how to set up a Linux operating system first.  

### Ubuntu Linux Live USB Setup

I created a bootable USB to run Ubuntu Linux without installation on my (otherwise Windows) PC following the Ubuntu [toturial](https://ubuntu.com/tutorials/tutorial-create-a-usb-stick-on-ubuntu#1-overview). A persistent storage (> 100 GB) was created with the help from [UUI](https://www.pendrivelinux.com/universal-usb-installer-easy-as-1-2-3) and a set of online tutorials (see below). The trick is to create a very small (10 MB) casper-rw file while installing Ubuntu on the USB, and then use Ubuntu's GParted partition manager to create a large (> 100 GB) casper-rw partition on the USB drive (_but not to delete the casper-rw file after creating the casper-rw partition_). This solution to persistent storage is somewhat both a mixture of and a deviation from the tutorials on [Stack Exchange](https://askubuntu.com/questions/397481/how-to-make-a-persistent-live-ubuntu-usb-with-more-than-4gb) and [How-to Geek](https://www.howtogeek.com/howto/14912/create-a-persistent-bootable-ubuntu-usb-flash-drive/). The detais are as follows:

* First I got two USB drives, one 300 GB, the other 4 GB.
* I then, on my Windows PC, downloaded and installed the lastest Ubuntu ISO image on both USB drives, following the Ubuntu [toturial](https://ubuntu.com/tutorials/tutorial-create-a-usb-stick-on-ubuntu#1-overview). This tutorial uses Rufus but I used UUI instead since somehow I had problem downloading Rufus. It was important that _a 10 MB casper-rw file was created when installing Ubuntu on the 300 GB USB drive_.
* Then, the challenge was to open the [UEFI](https://www.windowscentral.com/how-enter-uefi-bios-windows-10-pcs) settings page while restarting the PC, with the 4 GB USB inserted and the 300 GB USB detached. I use an Acer laptop and the way that works for me is to quickly press the F2 key several times when the computer is just starting, i.e. when it is showing an Acer logo on the screen before the Microsoft Windows splash screen is shown. 
* From the UEFI settings page, I changed the boot mode from UEFI to Legacy, and increased the priority of the option of booting from USB. Switching bewteen the UEFI mode and the Legacy mode is needed to switch between Windows and Linux.
* After successfully launched Ubuntu from the 4 GB USB, I inserted the 300 GB USB, and made the EXT4 casper-rw partition on it.
* Then I restarted the computer to launch Ubuntu from the 300 GB USB.

The above sequence of actions is just a success trace. I had many failed attempts before this one was found to work. With a persistent storage all files downloaded during a Ubuntu live USB session would be stored and still be available at the next session.  
