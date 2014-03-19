(* 
	Amber Rastogi
	50097978
*)

CM.make "$cml/cml.cm";
open CML;

val chan1_2: int chan = channel();			
val chan2_3: int chan = channel();			

fun sender1 x y = if y = 0 then (
					send(chan1_2, x)
					)
				else (
						send(chan1_2, x);
						let 
							val newValFrom2 = recv(chan1_2)
						in (
							sender1 newValFrom2 (y-1)
						)
						end
					)

fun sender2 x = 
	let 
		val newValFrom1 = recv(chan1_2)
		val newVal = x+newValFrom1
	in (
		send(chan1_2, x);
		send(chan2_3, newValFrom1);
		sender2 (newVal)
		)
	end

fun receiver ()=
	let 
		val newVal = recv(chan2_3)
	in
		(
		TextIO.print(Int.toString(newVal) ^"\n");
		receiver())
	end	

fun main() = 
	let 
		val _ = spawn(fn() => (sender1 0 50)); 
		val _ = spawn(fn() => (sender2 1)); 
		val _ = spawn(receiver)
	in 
		()
	end;

RunCML.doit(main, SOME(Time.fromMilliseconds 10));
