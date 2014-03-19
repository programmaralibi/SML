(* 
	Amber Rastogi
	50097978
*)

CM.make "$cml/cml.cm";
open CML;

datatype 'a request = GET | PUT of 'a

val chan: int chan = channel();			

fun sender x = if x = 100 then 
						(send(chan, x))
					else
						(send(chan, x); sender (x+1));

fun receiver () = 
	let
		val newVal = recv(chan)
	in
	TextIO.print(Int.toString(newVal) ^ "\n");
	receiver()
end
	
fun main() =
	let 
		val _ = spawn(fn() => (sender 0)) ; 
		val _ = spawn (fn() => TextIO.print(Int.toString(receiver())))
	in ()
	end;
 
RunCML.doit(main, SOME(Time.fromMilliseconds 10));
