washi.lisp
==========

A simple waschi cl-i-ent written in CommonLisp (SBCL) - Licensed under the AGPLv3+ by d3f - identi.ca/d3f - twitter.com/_d3f

##Introduction:
This is a simple client for the famous waschi-network of @Meikodis - https://github.com/MeikoDis/waschi
Read more about waschi in general on their homepage: http://waschi.org/
You will also find other clients out there, feel free to use them, and let me know if you want any improvements.

##Requirements:
- A working CommonLisp (most should work - tested with SBCL)
- You'll need quicklisp to run(build) this programm http://www.quicklisp.org/beta/

##How-To:
Just run 'sbcl --load washi.lisp' everything else will be introduced on screen.
search - this will search in the network, and will try to fetch your object from the server.
clean - this will send your object to one server.
u - this will let you change your login data
take-all - this is somekind of 'bruteforce', and will try to get all objects on the servers with your login data.
quit - the title says it all.

