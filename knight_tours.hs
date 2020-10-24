

-- idea: use list monad to generate list of 
-- all places a leaper chess piece can get to in n moves
-- ultimately want to include (using the Writer monad) 
-- a list of the moves needed to get there also

type ChessPos = (Int, Int)
type ChessMove = (Int, Int)

boardHeight = 8
boardWidth = 8

-- leaper's move
leaperMovement = (1,2) :: ChessMove

-- relfects and rotates leaper's move 
-- according to all the symmetries of a square
leaperMoves :: ChessMove -> [ChessMove]
leaperMoves (a,b) = [(a,b), (b,a), (-a,b), (b,-a), (a,-b), (-b,a), (-a,-b), (-b,-a)]

leaperReachInOne :: ChessPos -> [ChessPos]
leaperReachInOne (x,y) = [(x+a, y+b) 
                                | (a,b) <- leaperMoves leaperMovement,
                                x+a >= 1, 
                                x+a <= boardHeight, 
                                y+b >= 1,
                                y+b <= boardWidth
                         ]

main = putStrLn $ show $ do
                            pos <- leaperReachInOne (1,1)
                            pos2 <- leaperReachInOne pos
                            pos3 <- leaperReachInOne pos2
                            return pos3

