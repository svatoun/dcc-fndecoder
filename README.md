# dcc-fndecoder

SW for PIC-based DCC function decoder for rolling stock. The code is based on function decoder v3, available from Paco Canada website (http://usuaris.tinet.cat/fmco/dccfunc_en.html#dccfunc). 

The original code is distributed only compiled and lacks e.g. CV-based decoder lock, so the vehicles equipped with this decoder cannot be POM-programmed (or at least you risk that the function decoder misinterprets instructions for the main one). This project aims to provide decoder lock and possibly other modifications.

Although I did contact the author/published (several times), I've received no response at all. I am going to code the project in good faith that contributions to the DCC community are encouraged, or at least not prohibited and the software is more or less "abandonware". Please be warned that the project may terminate at any time if the original author disapproves such use of the original work.

The __dcc_func_wagon.*__ files is the original software, saved for comparison and reference. __main.asm__ is a copy of dcc_func_wagon.asm.
