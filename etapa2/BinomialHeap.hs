module BinomialHeap where

import Data.Function (on)
import Data.List

{-
    Reprezentarea unui arbore binomial, având priorități de tipul p și chei
    de tipul k. Conform proprietății definitorii a acestor arbori, fiecare
    copil i dintre cei r copii ai unui nod de rang r, cu 1 <= i <= r, trebuie
    să aibă exact r-i copii la rândul său, adică r-1, r-2, ..., 1, 0 copii,
    exact în această ordine descrescătoare. Dacă rădăcina are r copii, atunci,
    în conjuncție cu proprietate anterioară, întregul arbore are 2^r noduri.
-}
data BinomialTree p k
    = EmptyTree
    | Node { prio :: p, key :: k, children :: [BinomialTree p k] }
    deriving (Show, Eq)

{-
    Reprezentarea unui heap binomial, ca listă de arbori binomiali ce respectă
    proprietatea de heap. Mai precis, considerăm că, în fiecare arbore binomial,
    rădăcina are cea mai mică prioritate; analog pt subarbori. Câmpul size
    desemnează numărul de elemente din heap (nu numărul de arbori). Mai precis,
    dimensiunea este egală cu suma dimensiunilor arborilor din listă. Cum
    dimensiunea unui arbore de rang r este 2^r, dimensiunea este o sumă
    de puteri ale lui 2, unde exponenții sunt dimensiunile câmpurilor children
    (rangurile) din rădăcinile arborilor nevizi.
-}
data BinomialHeap p k = BinomialHeap { size :: Int, trees :: [BinomialTree p k] }
    deriving (Show, Eq)

{-
    *** TODO ***

    Construiește recursiv un arbore binomial de rang r din doi arbori binomiali
    de rang r-1, atașând unul dintre arbori drept prim copil al rădăcinii
    celuilalt. Maniera în care se realizează atașarea trebuie să țină cont
    de faptul că cei doi arbori respectă proprietatea de heap, și că arborele
    rezultant trebuie de asemenea să o respecte. Astfel, arborele cu cheia mai
    mică din rădăcină trebuie să încorporeze arborele cu cheia mai mare.

    Atenție! Cei doi arbori primiți ca parametru au întotdeauna același rang,
    conform principiului de construcție. Funcția nu necesită parcurgeri
    recursive, putând opera direct la nivelul rădăcinilor.

    Constrângeri: utilizați gărzi.

    Hint: pt pattern matching, pot fi utile alias-urile (@).

    Exemple:

    > attach (Node 0 'a' []) (Node 1 'b' [])
    Node {prio = 0, key = 'a', children = [Node {prio = 1, key = 'b', children = []}]}

    > attach (Node 1 'b' []) (Node 0 'a' [])
    Node {prio = 0, key = 'a', children = [Node {prio = 1, key = 'b', children = []}]}
-}
attach :: Ord p => BinomialTree p k -> BinomialTree p k -> BinomialTree p k
attach t1@(Node p1 k1 c1) t2@(Node p2 k2 c2)
  | p1 < p2   = Node p1 k1 (t2 : c1)
  | otherwise = Node p2 k2 (t1 : c2)



{-
    *** TODO ***

    Introduce un arbore binomial nevid într-o listă cu alți arbori binomiali,
    toți arborii respectând proprietatea de heap. Cum nu pot exista simultan
    în listă doi arbori binomiali cu același rang, la întâlnirea unui arbore
    cu același rang cu cel care se dorește introdus, este necesară atașarea
    unuia la celălalt, cu crearea unui transport.

    Operația o oglindește pe cea de incrementare a unui număr binar din etapa 1.
    Astfel, elementele EmptyTree sunt analoagele biților 0, iar elementele Node,
    biților 1. O diferență este că, în această etapă, biții 1 „nu mai arată toți
    la fel”, ci elementele Node au rangul dat de poziția în listă. Spre exemplu:
    * un element Node de pe poziția 0 din listă trebuie să aibă rangul 0
      (dimensiunea 2^0 = 1)
    * un element Node de pe poziția 1 din listă trebuie să aibă rangul 1
      (dimensiunea 2^1 = 2)
    * un element Node de pe poziția 2 din listă trebuie să aibă rangul 2
      (dimensiunea 2^2 = 4)
    etc.

    Gestiunea transportului apărut în incrementare corespunde operației attach.
    Modul în care va fi utilizată mai departe funcția insertTree garantează
    respectarea presupunerilor funcției attach, cum că arborii primiți ca
    parametru au întotdeauna același rang.

    Constrângeri: utilizați
    * construcția case
    * funcția attach.

    Exemple:

    > insertTree (Node 1 'a' []) []
    [Node {prio = 1, key = 'a', children = []}]

    > insertTree (Node 2 'b' []) $ insertTree (Node 1 'a' []) []
    [ EmptyTree
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]

    > insertTree (Node 3 'c' []) $ insertTree (Node 2 'b' []) $ insertTree (Node 1 'a' []) []
    [ Node {prio = 3, key = 'c', children = []}
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]
-}
insertTree :: Ord p => BinomialTree p k -> [BinomialTree p k] -> [BinomialTree p k]
insertTree tree [] = [tree]
insertTree tree1 (tree2 : rest) = case (tree1, tree2) of 
    (EmptyTree, _) -> tree2 : rest
    (_ , EmptyTree) -> tree1 : rest
    (Node p1 k1 children1, Node p2 k2 children2)
        | p1 < p2 -> tree1 : tree2 : rest 
        | otherwise -> EmptyTree : insertTree (attach tree1 tree2) rest




{-
    *** TODO ***

    Heap-ul vid.
-}
emptyHeap :: BinomialHeap p k
emptyHeap = BinomialHeap 0 []

{-
    *** TODO ***

    Introduce o cheie cu prioritatea aferentă într-un heap binomial.

    Constrângeri: utilizați funcția insertTree.

    Exemple:

    > insert 1 'a' emptyHeap
    BinomialHeap 
        { size = 1
        , trees = [Node {prio = 1, key = 'a', children = []}]
        }

    > insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap
        { size = 2
        , trees = [ EmptyTree
                  , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
                  ]
        }

    > insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    BinomialHeap 
        { size = 3
        , trees = [ Node {prio = 3, key = 'c', children = []}
                  , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
                  ]
        }
-}
insert :: Ord p => p -> k -> BinomialHeap p k -> BinomialHeap p k
insert prio key heap = BinomialHeap (size heap + 1) (insertTree (Node prio key []) (trees heap))

{-
    *** TODO ***

    Dacă heap-ul nu este vid, întoarce perechea formată din prioritatea minimă
    și cheia aferentă; în caz contrar, întoarce Nothing. Cum toți arborii din
    listă respectă proprietatea de heap, este suficient să parcurgeți doar
    rădăcinile pt a o determina pe cea cu prioritate minimă, fără a fi necesară
    explorarea nivelurilor inferioare ale arborilor.

    Constrângeri: pt selectarea arborilor nevizi din listă (ignorând elementele
    EmptyTree), utilizați list comprehension cu pattern matching.

    Hint: pt determinarea elementului minim dintr-o listă pe baza valorii
    calculate de o funcție numită criteriu, utilizați o expresie de forma:
    minimumBy (compare `on` criteriu) lista.

    Exemple:

    > findMin emptyHeap
    Nothing

    > findMin $ insert 3 'c' $ insert 2 'b' $ insert 1 'a' emptyHeap
    Just (1,'a')
-}
findMin :: Ord p => BinomialHeap p k -> Maybe (p, k)
findMin t@(BinomialHeap size trees) = 
    case trees of
        [] -> Nothing
        _ -> let
                prioKeyPair =  [prioKeyPair | Node prio key children <- trees, let prioKeyPair = (prio, key)]
                getPriority (prio, _) = prio
            in Just (minimumBy (compare `on` getPriority) prioKeyPair)

{-
    Funcția zipExtend este similară funcției predefinite zip. Scopul ei este
    de a compensa limitarea funcției zip, care se oprește când atinge sfârșitul
    listei mai scurte. Primii doi parametri reprezintă valori cu care se extind
    prima, respectiv a doua listă, în funcție de care listă este mai scurtă.
    O puteți folosi în cele ce urmează.

    Exemple:

    > zipExtend 0 'z' [1,2] "abcd"
    [(1,'a'),(2,'b'),(0,'c'),(0,'d')]

    > zipExtend 0 'z' [1,2,3,4] "ab"
    [(1,'a'),(2,'b'),(3,'z'),(4,'z')]
-}
zipExtend :: a -> b -> [a] -> [b] -> [(a, b)]
zipExtend a' _  [] bs = zip (repeat a') bs
zipExtend _  b' as [] = zip as (repeat b')
zipExtend a' b' (a : as) (b : bs) = (a, b) : zipExtend a' b' as bs

{-
    *** TODO ***

    Combină două liste de arbori binomiali care respectă proprietatea de heap.
    Observațiile din comentariile funcției insertTree, legate de necesitatea
    atașării arborilor cu același rang, rămân valabile.

    Operația o oglindește pe cea de adunare a două numere binare din etapa 1.

    Constrângeri:
    * evitați recursivitatea explicită
    * utilizați funcția zipExtend pt a facilita aplicarea unor funcționale.

    Exemple:

    > mergeTrees [Node 1 'a' []] []
    [Node {prio = 1, key = 'a', children = []}]

    > mergeTrees [Node 1 'a' []] [Node 2 'b' []]
    [ EmptyTree
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]

    > mergeTrees [Node 3 'c' []] $ mergeTrees [Node 1 'a' []] [Node 2 'b' []]
    [ Node {prio = 3, key = 'c', children = []}
    , Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}
    ]
-}

degree :: BinomialTree p k -> Int
degree EmptyTree = -1  -- by convention
degree (Node _ _ children) = length children

-- mergeTrees :: Ord p => [BinomialTree p k] -> [BinomialTree p k] -> [BinomialTree p k]
-- mergeTrees [] tree2 = tree2
-- mergeTrees tree1 [] = tree1
-- mergeTrees tree1@(t1:tree1') tree2@(t2:tree2')
--     | degree t1 < degree t2 = t1 : mergeTrees tree1' tree2
--     | degree t2 < degree t1 = t2 : mergeTrees tree1 tree2'
--     | otherwise = insertTree (attach t1 t2) (mergeTrees tree1' tree2')

-- mergeTrees :: Ord p => [BinomialTree p k] -> [BinomialTree p k] -> [BinomialTree p k]
-- mergeTrees [] tree2 = tree2
-- mergeTrees tree1 [] = tree1
-- mergeTrees tree1 tree2
--     | length tree1 /= length tree2 = error "input lists must have the same length"
--     | otherwise = go (zipExtend EmptyTree EmptyTree tree1 tree2)
--     where
--         go [] = []
--         go ((tree1, tree2):ts')
--             | degree tree1 < degree tree2 = tree1 : go ts'
--             | degree tree2 < degree tree1 = tree2 : go ts'
--             | otherwise = insertTree (attach tree1 tree2) (go ts')


-- mergeTrees :: Ord p => [BinomialTree p k] -> [BinomialTree p k] -> [BinomialTree p k]
-- mergeTrees [] [] = []
-- mergeTrees ts1 ts2 = go (zipExtend EmptyTree EmptyTree ts1 ts2)
--   where
--     go [] = []
--     go [(tree, EmptyTree)] = [tree]
--     go [(EmptyTree, tree)] = [tree]
--     go ((tree1, EmptyTree) : ts') = tree1 : go ts'
--     go ((EmptyTree, tree2) : ts') = tree2 : go ts'
--     go ((tree1@(Node p1 k1 children1), tree2@(Node p2 k2 children2)) : ts') =
--       if p1 < p2
--         then tree1 : go ts'
--         else if p2 < p1
--           then tree2 : go ts'
--           else insertTree (attach tree1 tree2) (go ts')

mergeTrees :: Ord p => [BinomialTree p k] -> [BinomialTree p k] -> [BinomialTree p k]
mergeTrees [] [] = []
mergeTrees ts1 ts2 = go (zipExtend EmptyTree EmptyTree ts1 ts2)
  where
    go [] = []
    go [(tree, EmptyTree)] = [tree]
    go [(EmptyTree, tree)] = [tree]
    go ((tree1, EmptyTree) : ts') = tree1 : go ts'
    go ((EmptyTree, tree2) : ts') = tree2 : go ts'
    go ((tree1@(Node p1 k1 children1), tree2@(Node p2 k2 children2)) : ts') = case compare p1 p2 of
      LT -> tree1 : go ts'
      GT -> tree2 : go ts'
      EQ -> insertTree (attach tree1 tree2) (go ts')

-- mergeTrees :: Ord p => [BinomialTree p k] -> [BinomialTree p k] -> [BinomialTree p k]
-- mergeTrees [] [] = []
-- mergeTrees ts1 ts2 = go (zipExtend EmptyTree EmptyTree ts1 ts2)
--   where
--     go [] = []
--     go [(tree, EmptyTree)] = [tree]
--     go [(EmptyTree, tree)] = [tree]
--     go ((tree1, EmptyTree) : ts') = tree1 : go ts'
--     go ((EmptyTree, tree2) : ts') = tree2 : go ts'
--     go ((tree1@(Node p1 k1 children1), tree2@(Node p2 k2 children2)) : ts')
--       | p1 < p2 = tree1 : go ts'
--       | p2 < p1 = tree2 : go ts'
--       | otherwise = go $ insertTree (attach tree1 tree2) ts'

-- mergeTrees :: (Ord p, Ord k) => [(BinomialTree p k, BinomialTree p k)] -> [BinomialTree p k] -> [BinomialTree p k]
-- mergeTrees [] trees = trees
-- mergeTrees ((tree1, tree2) : ts') trees =
--     let
--         (Node p1 k1 children1, Node p2 k2 children2) = (tree1, tree2)
--         attachTree = attach tree1 tree2
--         ts'' = (attachTree, attachTree) : ts'
--     in
--         case compare p1 p2 of
--             LT -> tree1 : mergeTrees ts' trees
--             GT -> tree2 : mergeTrees ts' trees
--             EQ -> mergeTrees (insertTree ts'' []) trees
--     where
--         insertTree ts [] = ts
--         insertTree ((t1, t2) : ts') ((Node p k children) : trees') =
--             if p < fst (priority t1)
--             then (t1, t2) : (Node p k children) : ts' ++ trees'
--             else (Node p k children) : insertTree ts' trees'
--         insertTree ((t1, t2) : ts') [] = t1 : t2 : ts'


-- mergeTrees :: Ord p => [BinomialTree p k] -> [BinomialTree p k] -> [BinomialTree p k]
-- mergeTrees [] [] = []
-- mergeTrees ts1 ts2 = go (zipExtend EmptyTree EmptyTree ts1 ts2)
--   where
--     go [] = []
--     go [(tree, EmptyTree)] = [tree]
--     go [(EmptyTree, tree)] = [tree]
--     go ((tree1, EmptyTree) : ts') = tree1 : go ts'
--     go ((EmptyTree, tree2) : ts') = tree2 : go ts'
--     go ((tree1@(Node p1 k1 children1), tree2@(Node p2 k2 children2)) : ts')
--       | p1 < p2 = tree1 : go ts'
--       | p2 < p1 = tree2 : go ts'
--       | otherwise = go $ insertTree (attach tree1 tree2) ts'




{-
    *** TODO ***

    Combină două heap-uri binomiale.

    Constrângeri: utilizați funcția mergeTrees.

    Exemple: similare cu cele de la mergeTrees.
-}
merge :: Ord p => BinomialHeap p k -> BinomialHeap p k -> BinomialHeap p k
merge heap1 heap2 = BinomialHeap (size heap1 + size heap2) (mergeTrees (trees heap1) (trees heap2))
