type 'a parser = char list -> ('a * char list) list
   
let return: 'a -> 'a parser = fun (a: 'a) (cs: char list) -> [(a, cs)]
 
let bind (p: 'a parser) (f: 'a -> 'b parser): ('b parser) = fun (cs: char list) ->
  p cs
  |> List.map (fun (a, cs') -> (f a) cs')
  |> List.concat
   
let (>>=) (p: 'a parser) (f: 'a -> 'b parser) = bind p f
   
let zero: char parser = fun cs -> []
                                 
let (++) (p: 'a parser) (q: 'a parser) = fun cs ->
  p cs @ q cs
 
let (+++) (p: 'a parser) (q: 'a parser): 'a parser = fun cs ->
  match (p ++ q) cs with
  | [] -> []
  | hd::_ -> [hd]
 
let item: char parser = function
  | [] -> []
  | hd :: tl -> [(hd, tl)]
               
let sat p =
  item >>= fun c ->
  if p c then return c else zero
   
let char c = sat (fun c' -> c = c')
   
let rec string = function
  | [] -> return []
  | c::cs ->
      (char c) >>= fun _ ->
      (string cs) >>= fun _ ->
      return (c::cs)
 
let rec many (p: 'a parser) =
  many1 p +++ return []
 
and many1 (p: 'a parser) =
  p >>= fun a ->
  many p >>= fun as' ->
  return (a::as')
 
let rec sepby (p: 'a parser) (sep: 'b parser) =
  sepby1 p sep +++ return []
   
and sepby1 (p: 'a parser) (sep: 'b parser) =
  p >>= fun a ->
  many (sep >>= fun _ -> p) >>= fun as' ->
  return (a::as')