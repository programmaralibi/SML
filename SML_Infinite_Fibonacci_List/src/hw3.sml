(* 
	Amber Rastogi
	amberras@buffalo.edu
	50097978
*)

Control.Print.printLength := 100;

datatype 'a inflist = NIL
                    | CONS of 'a * (unit -> 'a inflist);

  					
fun HD (CONS(a,b)) = a
  | HD NIL = raise Subscript;

fun TL (CONS(a,b)) = b()
  | TL NIL = raise Subscript;

fun NULL NIL = true
  | NULL _ = false;

fun FILTER f l = if NULL l
                 then NIL
                 else if f (HD l)
                      then CONS(HD l, fn () => 
                                         (FILTER f (TL l)))
                      else FILTER f (TL l);

fun TAKE (xs, 0 )  =  [] 
  | TAKE (NIL,n)   = raise Subscript
  | TAKE (CONS(x,xf), n)  = x::TAKE(xf(), n-1) ;

fun even (n) = if (n mod 2 = 0) then true else false;
fun odd (n) = if (n mod 2 = 1) then true else false;
  
fun fib m n = CONS(m , fn () => fib n (m+n)); 

val fibs = fib 0 1;

val evenFibs = FILTER even fibs; 
val oddFibs = FILTER odd fibs; 

fun printGenList f l = if null l
						then ()
						else (
							f(hd l);
							printGenList f (tl l)
						);

fun printList l = printGenList (fn(x) => print(Int.toString(x) ^ " ")) l;						
fun printPairList l = printGenList(fn(x,y) => print(" (" ^ Int.toString(x) ^ ", " ^ Int.toString(y) ^ ")")) l;						

fun ZIP (NIL, NIL) = NIL
  | ZIP (L1, L2) =
      let val a = HD L1 and b = HD L2
      in CONS((a,b), fn () =>
                    ZIP(TL L1, TL L2))
      end;

printList(TAKE (fibs, 20));
printList(TAKE (evenFibs, 10));
printList(TAKE (oddFibs, 10));
printPairList(TAKE (ZIP(evenFibs, oddFibs), 10));