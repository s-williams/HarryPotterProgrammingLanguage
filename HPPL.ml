
(* Save things to type envirnoment *)

(* Convert String to int -> Throw error if not valid string *)

(* Convert String to list -> Throw error if not valid string *)

(* Take a list and remove duplicates *)
let remove_duplicates aList = remove_item [] aList;;

(* Is a list longer than x 
 * 
 * @param list the list you wish to compare the length of
 * @param integer a number, x, you wish to compare the length of the list to
 * @return boolean true if list length is greater than x, false otherwise
 *)
let is_longer aList x =
	if List.length aList > x then true
	else false;;

(* Return the k element of a list 
 *
 * @param list the list you want an element of
 * @param integer the element index you want to retrieve
 * @return integer  the kth element of the list
 *)
let rec at k = function
    | [] -> None
    | h :: t -> if k = 1 then Some h else at (k-1) t;;
	
(* Take a list of string and concatenate each member with a given string at the front 
 *
 * @param list the list you wish to process
 * @param string the string you wish to concatenate with every element
 * @return list a list of strings with every element the same as the input list but concatenated with the string
 *)
let rec prefix aList aString = match aList , aString with
	[] , string -> []
	| (l :: ls) , aString -> (aString ^ l) :: prefix ls aString;;
	
(* Sort list into alphabetical order 
*
* @param list the list you wish to sort
* @return list the list sorted alphabetically
*)

(* Print list 
 * 
 * @param list the list you wish to print
 *)

(* Union two lists
 *
 * @param list the first list
 * @param list the second list
 * @return list the union of the first list and the second list
 *)
let union firstList secondList = remove_duplicates (append firstList secondList);;

(* Append one list onto the end of another
 * 
 * @param list the first list
 * @param list the second list
 * @return list a single list composed of the first list followed by the second list
 *)
let append l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc
    | [], h :: t -> loop (h :: acc) [] t
    | h :: t, l -> loop (h :: acc) t l
    in
    loop [] l1 l2

(* Intersect two lists *)

(* get x* up to k *)

(* Remove item from list *)
let rec remove_item listLeft listRight = match listRight with
	[] -> listLeft
	| (l :: ls) -> begin
		if is_member l listLeft then remove_item listLeft ls
		else remove_item (l :: listLeft) listRight end;;

(* Check if an elem is in a list *)
let rec is_member n aList = match aList with
	[] -> false
	| (l :: ls) -> begin
		if l = n then true
		else is_member n ls end;;
	
(* Take a list and concatenate each member with a given string at the end *)
let rec suffix aList aString = match aList , aString with
	[] , string -> []
	| (l :: ls) , aString -> (l ^ aString) :: suffix ls aString;;

(* TODO Add an element to the end of a list *)
let addToList aList anElement = match aList, anElement with
	[], anElement -> []
	| ();;

(* Adds an element at a specific point in a list *)
let rec insert_at x n = function
    | [] -> [x]
    | h :: t as l -> if n = 0 then x :: l else h :: insert_at x (n-1) t;;

(* Return the first k elements of a list- fail if k is bigger than list *)
let rec get_list_of_size k xs = match xs with
	| [] -> failwith "firstk"
	| x::xs -> if k=1 then [x] else x::get_list_of_size (k-1) xs;;