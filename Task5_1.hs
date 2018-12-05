module Task5_1 where

import Todo(todo)

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec

index :: DList a -> Int -> a
index DNil _ = error "Emptyness inside the list :("
index dlist idx = if idx < 0 then error "idx is not positive enough!"
                              else baseIndex dlist idx where
    baseIndex (DCons _ cur _) 0 = cur
    baseIndex (DCons DNil _ DNil) idx | idx /= 0 = error "Out of bounds"
    baseIndex (DCons _ _ r) ind = baseIndex r (ind - 1)


flattenLeft :: DList a -> DList a
flattenLeft DNil = DNil
flattenLeft normalized@(DCons DNil _ _) = normalized
flattenLeft (DCons (DCons ll lv lr) cur right) = DCons ll lv (DCons lr cur right)


insertAt :: DList a -> Int -> a -> DList a
insertAt list index value = if index < 0 then error "index is too negative!"
                              else browseL (flattenLeft list) index value where
browseL DNil i insertion | i == 0 = DCons DNil insertion DNil
                         | otherwise = error "End of list"
browseL (DCons DNil cur right) 0 insertion = DCons DNil insertion (DCons DNil cur right)
browseL (DCons DNil cur right) index insertion = DCons DNil cur (browseL right (index-1) insertion)


removeAt :: DList a -> Int -> DList a
removeAt list index = remover (flattenLeft list) index where
  remover DNil _ = error "Emptyness inside the list :("
  remover (DCons DNil cur DNil) index | index == 0 = DNil
                                      | otherwise = error "No such index"
  remover (DCons DNil cur (DCons DNil rc rr)) 0 = DCons DNil rc rr
  remover (DCons DNil cur right) index = if (index < 0) then error "negative!" else 
                      DCons DNil cur (remover right (index-1)) 
