(* 
	Amber Rastogi
	50097978
*)

CM.make "$cml/cml.cm";
open CML;

datatype 'a request = GET | PUT of 'a
			
fun mailbox inCh outCh list =  
	(
		if null list then
		(
			mailbox inCh outCh [recv(inCh)]		
		)
		else (
			select [
				wrap (recvEvt inCh, fn y => (mailbox inCh outCh (list@[y]))),
				wrap (sendEvt(outCh, hd list), fn () => (mailbox inCh outCh (tl list)))
			]
		)
	)

fun receiver outCh = 
	let
		val newVal = recv(outCh)
	in
	TextIO.print(Int.toString(newVal) ^ "\n");
	receiver outCh
end

fun sender inCh num = 
	(
	if num <> 0 then (
		send(inCh, num);
		sender inCh (num-1)
	)
	else (
		send(inCh, num)
	)	
	);


fun generator inCh outCh num = 
	if num = 0 then inCh
	else (
		let 
			val newCh = channel()
		in (
			spawn(fn () => mailbox inCh outCh nil ); 
			generator outCh newCh (num-1)
		)
		end
	);

fun main () =
  let val chStart = channel()
	val chEnd = generator chStart (channel()) 100
	val _ = spawn (fn () => receiver chEnd)
	val _ = spawn (fn () => ignore (sender chStart 50))
	val _ = spawn (fn () => ignore (sender chStart 50)) 
  in ()
  end;

RunCML.doit(main, SOME(Time.fromMilliseconds 10));

