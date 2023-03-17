type 'num t

val make :
  to_num:(int -> 'num) ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  'num t

val zeros : to_num:(int -> 'num) -> 'num t

val i : 'num t -> at:int -> 'num
