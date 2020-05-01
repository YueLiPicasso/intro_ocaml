# intro_ocaml
Artifects created while learning the OCaml language

[ch1](ch1), ch2, etc. corresponds to chapter 1, 2, ... of the OCaml [reference manual](http://caml.inria.fr/pub/docs/manual-ocaml/).

[summer](summer) contains workouts according to a summer school [tutorial](http://caml.inria.fr/pub/docs/u3-ocaml/index.html) by Didier Remy. 

[from_beginning](from_beginning) are created following a [textbook](http://ocaml-book.com/) by John Whitington.

## Installation Notes

I set up OCaml following the instructions from [Real World Ocaml](http://dev.realworldocaml.org/install.html). OCaml is very Windows-unfriendly, so I had to learn how to set up a Linux operating system first.  

I created a bootable USB to run Ubuntu Linux without installation on my (otherwise Windows) PC following the Ubuntu [toturial](https://ubuntu.com/tutorials/tutorial-create-a-usb-stick-on-ubuntu#1-overview). A persistent storage was created with the help from [UUI](https://www.pendrivelinux.com/universal-usb-installer-easy-as-1-2-3). The trick is to create a very small (10 MB) casper-rw file while installing Ubuntu on the USB, and then use Ubuntu's GParted partition manager to create a casper-rw partition on the USB drive ( _but not to delete the casper-rw file after creating the casper-rw partition_ ). This solution to persistent storage is somewhat both a mixture of and a deviation from the tutorials on [Stack Exchange](https://askubuntu.com/questions/397481/how-to-make-a-persistent-live-ubuntu-usb-with-more-than-4gb) and [How-to Geek](https://www.howtogeek.com/howto/14912/create-a-persistent-bootable-ubuntu-usb-flash-drive/).
