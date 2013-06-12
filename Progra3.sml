(*
INSTITUTO TECNOLÓGICO DE COSTA RICA

PROGRAMA EXTRAER FECHAS EN STANDARD ML OF NEW JERSEY
PROYECTO III LENGUAJES DE PROGRAMACION

INTEGRANTES: Andrew Araya, Benjamín Calvo, Alexander Durán. *)


Control.Print.printLength := 10000;
Control.Print.stringDepth := 10000000;

(*FUNCION: Analiza si un año determinado es un año esBisiesto*)
fun esBisiesto ( fecha : int*int*int)=
    if (#3 fecha mod 4) = 0
    then
		if (#3 fecha mod 100) = 0
		then
			if (#3 fecha mod 400)=0
			then true
			else false
		else true
    else false 

(*FUNCION: comprueba si los dias se pueden cumplir para el mes indicado*)
fun validar (fecha : int*int*int)=
    let fun validaMes30 (x : int, lista : int list)=
	    if lista = []
	    then false
	    else
		if hd lista = x
		then true
		else validaMes30 (x,tl lista)
    in
	if 0 < (#1 fecha) andalso (#1 fecha) <= 31 andalso 0 < (#2 fecha) andalso (#2 fecha)  <= 12 andalso 0 < (#3 fecha) andalso (#3 fecha) <= 9999
	then
	    if (#2 fecha) = 2
	    then 
		if esBisiesto(fecha)
		then
		    if (#1 fecha)<=29
		    then true
		    else false
		else
		    if (#1 fecha)<=28
		    then true
		    else false
	    else
		if validaMes30(#2 fecha,[4, 6, 9, 11])
		then
		    if (#1 fecha)<=30
		    then true
		    else false
		else
		    if (#1 fecha)<=31
		    then true
		    else false
	else false
    end;

(*FUNCION: traducir la fecha en formato solicitado ej.: 12 de febrero del 2013*)
fun pasarATexto (fecha : int*int*int)=
    let
	fun getMes (m: string list,indice:int)=
	    if indice=1 
	    then hd m
	    else getMes(tl m, indice-1)
	val meses : string list = ["Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Setiembre","Octubre","Noviembre","Diciembre"]
	val mes = getMes(meses,#2 fecha)
    in    
	if (#3 fecha) div 1000 =1
	then
	    (Int.toString (#1 fecha) ^ " de " ^ mes ^ " de " ^ Int.toString (#3 fecha))
	else
	    (Int.toString (#1 fecha) ^ " de " ^ mes ^ " del " ^ Int.toString (#3 fecha))
    end;

(*FUNCION: Recibe un string de formato dd/mm/yy o dd/mm/yyyy y lo convierte en un vector de tipo int*int*int para la lista de fechas*)
fun textoAEntero (fecha : string)=
    if String.size fecha = 8
    then 
	let 
	    fun valOptionAEntero (x:int option) = case x of SOME int => int | NONE => 0; 
	    val dia = (valOptionAEntero((Int.fromString((Char.toString(String.sub (fecha,0))^Char.toString(String.sub (fecha,1)))))))
	    val	mes = (valOptionAEntero((Int.fromString(Char.toString(String.sub (fecha,3))^Char.toString(String.sub (fecha,4))))))
	    val anio = (valOptionAEntero((Int.fromString(Char.toString(String.sub (fecha,6))^Char.toString(String.sub (fecha,7))))))
	    val tupla : int*int*int = (dia,mes,anio)

	in
	    tupla
	end
    else
	let
	    fun valOptionAEntero (x:int option) = case x of SOME int => int | NONE => 0; 
	    val dia = (valOptionAEntero((Int.fromString((Char.toString(String.sub (fecha,0))^Char.toString(String.sub (fecha,1)))))))
	    val	mes = (valOptionAEntero((Int.fromString(Char.toString(String.sub (fecha,3))^Char.toString(String.sub (fecha,4))))))
	    val anio = (valOptionAEntero((Int.fromString(Char.toString(String.sub (fecha,6))^Char.toString(String.sub (fecha,7))^Char.toString(String.sub (fecha,8))^Char.toString(String.sub (fecha,9))))))
	    val tupla : int*int*int = (dia,mes,anio)
	in 
	    tupla
	end;

(*FUNCION: Valida que un string ingresado sea una fecha permitida y devuelve true o false*)
fun validaFecha (fecha : string)=
    let 	
		val fechaArreglo = textoAEntero(fecha)
    in 
		if validar(fechaArreglo)
		then true
		else false
    end;


(*FUNCION: Analizador de fechas en el String creado del archivo, esta función recorre el string para extraer las fechas que se encuentren en el mismo y devuelve in valor tipo (int*int*int) list option con las fechas contenidas en el*)
fun analizarFecha(c1:char,c2:char)=
    if c1=c2 
	then true 
	else false;

fun capturar (texto:string,largo:int)=
    if largo<8
    then NONE
    else
    let 
		val lista = []
		fun aux(indice:int,contador:int,lista: (int*int*int) list)=
			if indice<largo 
			then
			if contador=0 orelse contador=1 orelse contador=3 orelse contador=4 orelse contador=6 orelse contador=7 then
				if Char.isDigit(String.sub(texto,indice)) then 
				(*print((Char.toString(String.sub(texto,indice)))^" cumple con: ser numero que suma. Contador = "^Int.toString(contador)^" \n");*)
				aux(indice+1,contador+1,lista)
				else
				(*print((Char.toString(String.sub(texto,indice)))^" no es un digito. Contador = 0 \n");*)
				aux(indice+1,0,lista)
			else
				if contador=2 orelse contador=5 then
				if (analizarFecha(String.sub(texto,indice),#"/")) then
					(*print((Char.toString(String.sub(texto,indice)))^" cumple con: ser '/' que suma. Contador = "^Int.toString(contador)^" \n");*)
					aux(indice+1,contador+1,lista)
				else
					(*print((Char.toString(String.sub(texto,indice)))^" no es un digito. Contador = 0 \n");*)
					aux(indice+1,0,lista)
				else
				if contador=8 then
					if (Char.isDigit(String.sub(texto,indice))) then
					aux(indice+1,contador+1,lista)
					else
					if (Char.isSpace(String.sub(texto,indice))) then
						if (validaFecha((String.substring(texto,indice-8,8)))) then
						let 
							val temporal=textoAEntero(String.substring(texto,indice-8,8))
						in
							aux(indice+1,0,temporal::lista)
						end
						else
						let
							val texto = "La fecha: "^String.substring(texto,indice-8,8)^" es invalida."
						in
							(*print(texto);*)
							aux(indice+1,0,lista)
						end
					else
							aux(indice+1,0,lista)
				else
					if contador=9 then
					if (Char.isDigit(String.sub(texto,indice))) then
						aux(indice+1,contador+1,lista)
					else
						if (Char.isSpace(String.sub(texto,indice))) then
						if validaFecha((String.substring(texto,indice-9,8))) then
							let 
							val temporal=textoAEntero(String.substring(texto,indice-9,8))
							in
							aux(indice+1,0,temporal::lista)
							end
						else
							let
							val texto = "La fecha: "^String.substring(texto,indice-9,8)^" es invalida"
							in
							(*print (texto);*)
							aux(indice+1,0,lista)
							end
					else
							aux(indice+1,0,lista)


					else
					if contador=10 then
						if (Char.isSpace(String.sub(texto,indice))) then
						if validaFecha((String.substring(texto,indice-10,10))) then
							let 
								val temporal=textoAEntero(String.substring(texto,indice-10,10))
							in
								aux(indice+1,0,temporal::lista)
							end
						else
							let
							val texto = "La fecha: "^String.substring(texto,indice-10,10)^" es invalida. \n"
							in
							(*print(texto);*)
							aux(indice+1,0,lista)
							end
						else
						aux(indice+1,0,lista)
					else
						aux(indice+1,0,lista)
				else
					if (contador=8) then
						if validaFecha((String.substring(texto,indice-8,8))) then
						let 
							val temporal=textoAEntero(String.substring(texto,indice-8,8))
						in
							aux(indice+1,0,temporal::lista)
						end
						else
						let
							val texto = "La fecha: "^String.substring(texto,indice-8,8)^" es invalida";
						in
							(*print (texto);*)
							aux(indice+1,0,lista)
						end
					else
						if contador=9 then
						lista
						else
						if contador=10 then
							let 
								val temporal=textoAEntero(String.substring(texto,indice-10,10))
							in
								aux(indice+1,0,temporal::lista)
							end
						else
							let
							val texto = "La fecha: "^String.substring(texto,indice-11,10)^" es invalida.\n"
							in
							(*print(texto);*)
							lista
							end
		in
		    SOME (aux(0,0,lista))
		end;

(*FUNCION: Para crear string a partir del texto del archivo txt*)
fun abrirArchivo (file : string) = 
    let
	val instream = TextIO.openIn file 
	fun loop instream = 
	    case TextIO.inputLine instream of 
		SOME line => line^loop instream 
	      | NONE      => ("") 
    in 
	loop instream before TextIO.closeIn instream 
    end;

(*FUNCION: Obtiene una lista de tipo list a partir de una de tipo list option*)
fun valOptionALista (pLista)=
    let   
	val listaResultado = case pLista of SOME pLista => pLista | NONE => []
    in
	listaResultado
    end;

(*FUNCION: Obtiene la lista de tuplas final que contiene todas las fechas validas*)
fun getListaTuplas (nombre : string)=
    let 
		val texto = abrirArchivo(nombre)
		val largoTexto = String.size(texto)
		val listaFechas = capturar(texto,largoTexto)
		val listaFinal = valOptionALista(listaFechas)
	in
	listaFinal
    end;


(*
fun ordenarlista(lista)=
	if NULL lista
	then []
	else 
		if tl (lista) = []
		then lista
		else
			if
	*)

(* Funciones para ordenar listas de elementos enteros *)
fun take(L) =
    if L = nil 
	then nil
    else hd(L)::skip(tl(L))
and
	skip(L) =
    if L=nil 
	then nil
    else take(tl(L));

fun merge([],M) = M
  | merge(L,[]) = L
  | merge(x::xl,y::yl) =
    val valorx = #3 x;
    val valory = #3 y;
    if  valorx < valory
    then x::merge(xl,y::yl)
    else y::merge(x::xl,yl);

fun sort(L) =
    if L=[] 
    then []
    else 
	if tl(L)=[] 
	then L
	else merge(sort(take(L)),sort(skip(L)));

	
(********************************************ESTA ES LA FUNCION PRINCIPAL QUE DEBEMOS EJECUTAR**********************************************)
(*El argumento que debemos enviarle es el nombre del archivo que deseamos analizar ej.:   <<  archivo.txt  >>                              *)
(*FUNCION: Me devuelve la lista de tuplas final a partir de la función getListaTuplas() y me devuleve una lista de las fechas pero traducidas*)
fun extraerFechas (archivo : string)=
    let 
		val lista = getListaTuplas(archivo)
		(*aca va el ordenamiento*)
		val listaOrdenada = sort(lista)
		fun convertirLista (listaOrdenada)=
			if null listaOrdenada
			then []
			else  (pasarATexto(hd listaOrdenada) :: convertirLista(tl listaOrdenada))
		val listaFechasValidas = convertirLista(listaOrdenada)
    in
		listaFechasValidas
    end;
