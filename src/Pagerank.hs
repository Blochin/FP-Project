import           Data.Map    (Map, empty, insert, insertWith, lookup,
                              mapWithKey, member, size)
import           Data.Maybe  (fromJust)
import           Debug.Trace (trace)
import           Prelude     hiding (lookup)
import           Text.Printf (printf)


type Node = Int
type PRValue = Doubleype PageRank = Map Node PRValue
type InboundEdges = Map Node [Node]
type OutboundEdges = InboundEdges

-- ZDROJ: 
-- https://github.com/derekchiang/Haskell-Page-Rank
-- https://www.youtube.com/watch?v=xJUwspSaR1E&ab_channel=AnuradhaBhatia
-- TESTOVANIE: 
-- https://computerscience.chemeketa.edu/cs160Reader/_static/pageRankApp/index.html


parseLine :: (InboundEdges, OutboundEdges, Node) -> String -> (InboundEdges, OutboundEdges, Node)
parseLine (iEdges, oEdges, maxNode) line =
    -- zo suboru parsujeme vstup a ukladame jednotlive hrany do inbound a outbound edges
    let ws = words line
        (from, to) = (read $ ws !! 0, read $ ws !! 1)
        in (insertWith plusNode to [from] iEdges,
            insertWith plusNode from [to] oEdges,
            max to (max maxNode from))
    where
        plusNode :: [Node] -> [Node] -> [Node]
        plusNode new_node old_node =
            new_node ++ old_node

newPageRank :: Int -> PageRank
newPageRank n =
    -- fromIntegral len konvertuje integral value na general value
    let v :: Double; v = 1 / (fromIntegral n)
        in go n v empty
    where
        go :: Int -> Double -> PageRank -> PageRank
        go 0 _ pr = pr

        go n v pr =
            go (n-1) v $ insert (n-1) v pr 
        

-- The goal of postProcess is to deal with the nodes that have no outbound
-- edges, in which case they should be treated like they have outbound edges
-- to every other node. 
-- ?????? neviem co je toto za bullshit
-- tu prechadzame vsetkymi in/out edges 
postProcess :: (InboundEdges, OutboundEdges, Node) -> (InboundEdges, OutboundEdges)
postProcess (iEdges, oEdges, maxNode) =
    let numNodes = maxNode + 1
        newIEdges = addAllNodes (numNodes-1) iEdges
        in loop (numNodes-1) newIEdges oEdges

    where
        loop :: Int -> InboundEdges -> OutboundEdges -> (InboundEdges, OutboundEdges)
        loop n iEdges oEdges
            | n < 0 = (iEdges, oEdges)
            | otherwise =
                if member n oEdges then
                    loop (n-1) iEdges oEdges
                else
                    let numNodes = maxNode + 1
                        newOEdges = insert n (filter (/= n) [0..maxNode]) oEdges
                        newIEdges = mapWithKey (\k v -> if k /= n then v ++ [n] else v) iEdges
                        in loop (n-1) newIEdges newOEdges

        -- This function makes sure that every node is a key in the InboundEdges map
        addAllNodes :: Int -> InboundEdges -> InboundEdges
        addAllNodes n iEdges
            | n < 0 = iEdges
            | otherwise =
                addAllNodes (n-1) $ insertWith (\new old -> new ++ old) n [] iEdges


parseGraph :: String -> (InboundEdges, OutboundEdges, PageRank)
parseGraph input =
    let ls = lines input
        (iEdges, oEdges) = postProcess $ foldl parseLine (empty, empty, 0) ls
        numNodes = size iEdges
        in (iEdges, oEdges, newPageRank numNodes)


loopProcess :: Int -> Double -> InboundEdges -> OutboundEdges -> PageRank -> PageRank
loopProcess 0 _ _ _ pageRank = pageRank
-- ak je v loopProcess numIters == 0, vratime PR, inak robime rekurziu a vraciame PR
loopProcess n dampingFactor iEdges oEdges pageRank =
    let newPageRank = loop' ((size pageRank) - 1) empty
        in loopProcess (n-1) dampingFactor iEdges oEdges newPageRank

    where
        loop' :: Int -> PageRank -> PageRank
        loop' n pr
            | n < 0 = pr
            | otherwise =
                -- If the argument is Just, it returns the Just value, otherwise an error is produced
                -- napr. fromJust (lookup 3 [(1,'A'),(2,'B'),(3,'C')]) OUTPUT: 'C'
                -- napr. fromJust (lookup 31 [(1,'A'),(2,'B'),(3,'C')]) OUTPUT: Nothing - Error
                -- v nasom pripade pozerame ci n je v iEdges
                let inbounds = fromJust $ lookup n iEdges
                    -- VZOREC pouzity pre vypocet novej PR
                    -- PR(B) = ( (1-d)/N ) + ( d * (PR(A)/C(A) + ...) )
                    -- pricom:
                    ---- d == dampingFactor
                    ---- PR(A) == kolko inbounding ide do A?), postupne s iteraciami dosadzame PR 
                    ---- C(A) == kolko outbounding ide z A?
                    -- PR = (1-0.85)+0.85*(x/y)
                    newPrValue = (+)
                        ((1 - dampingFactor) / (fromIntegral $ size iEdges))
                        (dampingFactor * (foldl calc 0 inbounds))
                    -- rekurzivne insertujeme
                    in loop' (n-1) $ insert n newPrValue pr 
                where
                    calc acc node =
                        -- iba ak sa node nachadza v outbounding edges
                        let outbounds = fromJust $ lookup node oEdges
                            -- a pouzi pagerank ak sa pre node nachadza v pagerankoch
                            prValue = fromJust $ lookup node pageRank
                            in acc + prValue / (fromIntegral $ length outbounds)

-- inputFile numIters dampingFactor -> PR
-- Use let without in in the body of a do-block, and in the part after the | in a list comprehension. Anywhere else, use let ... in ...
startPagerank :: String -> Int -> Double -> PageRank
startPagerank inputFile numIters dampingFactor =
    -- parseGraph ma na vstupe .txt a na vystupe inbounding, outbounding a PR, ktory dalej pouzijeme v loopProcess
    let (iEdges, oEdges, pageRank) = parseGraph inputFile
        in loopProcess numIters dampingFactor iEdges oEdges pageRank

main :: IO ()
main = do
    let numIters = 10
    -- pri algoritme pagrank sa zvykne pouzivat 0.85 alebo 0.8
    let dampingFactor = 0.85
    -- v txt mame ulozeny graf - indexy stranok vo formate "'odkial' 'kam'" 
    inputFile <- readFile "input2.txt"
    -- na vstupe mame inputFile, pocet iteracii a damping factor nastaveny ako konstantu 0.85
    writeFile "output.txt" $ show $ startPagerank inputFile numIters dampingFactor 