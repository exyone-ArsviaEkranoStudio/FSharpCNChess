open System

// 定义棋子类型和颜色
type PieceType = 
    | General | Advisor | Elephant | Horse | Chariot | Cannon | Soldier

type Color = 
    | Red 
    | Blue

// 定义棋子
type Piece = {
    Type: PieceType
    Color: Color
}

// 定义棋盘位置
type Position = int * int

// 定义棋盘类型
type Board = Map<Position, Piece>

// 初始化棋盘
let initialBoard: Board = 
    let redPieces = [
        ((0, 0), { Type = Chariot; Color = Red })
        ((0, 1), { Type = Horse; Color = Red })
        ((0, 2), { Type = Elephant; Color = Red })
        ((0, 3), { Type = Advisor; Color = Red })
        ((0, 4), { Type = General; Color = Red })
        ((0, 5), { Type = Advisor; Color = Red })
        ((0, 6), { Type = Elephant; Color = Red })
        ((0, 7), { Type = Horse; Color = Red })
        ((0, 8), { Type = Chariot; Color = Red })
        ((2, 1), { Type = Cannon; Color = Red })
        ((2, 7), { Type = Cannon; Color = Red })
        ((3, 0), { Type = Soldier; Color = Red })
        ((3, 2), { Type = Soldier; Color = Red })
        ((3, 4), { Type = Soldier; Color = Red })
        ((3, 6), { Type = Soldier; Color = Red })
        ((3, 8), { Type = Soldier; Color = Red })
    ]
    
    let BluePieces = [
        ((9, 0), { Type = Chariot; Color = Blue })
        ((9, 1), { Type = Horse; Color = Blue })
        ((9, 2), { Type = Elephant; Color = Blue })
        ((9, 3), { Type = Advisor; Color = Blue })
        ((9, 4), { Type = General; Color = Blue })
        ((9, 5), { Type = Advisor; Color = Blue })
        ((9, 6), { Type = Elephant; Color = Blue })
        ((9, 7), { Type = Horse; Color = Blue })
        ((9, 8), { Type = Chariot; Color = Blue })
        ((7, 1), { Type = Cannon; Color = Blue })
        ((7, 7), { Type = Cannon; Color = Blue })
        ((6, 0), { Type = Soldier; Color = Blue })
        ((6, 2), { Type = Soldier; Color = Blue })
        ((6, 4), { Type = Soldier; Color = Blue })
        ((6, 6), { Type = Soldier; Color = Blue })
        ((6, 8), { Type = Soldier; Color = Blue })
    ]
    
    (redPieces @ BluePieces) |> Map.ofList

// 获取棋子的显示字符
let getPieceDisplay piece =
    match piece.Type, piece.Color with
    | General, Red -> "帅"
    | General, Blue -> "将"
    | Advisor, Red -> "仕"
    | Advisor, Blue -> "士"
    | Elephant, Red -> "相"
    | Elephant, Blue -> "象"
    | Horse, Red -> "马"
    | Horse, Blue -> "馬"
    | Chariot, Red -> "车"
    | Chariot, Blue -> "車"
    | Cannon, Red -> "炮"
    | Cannon, Blue -> "砲"
    | Soldier, Red -> "兵"
    | Soldier, Blue -> "卒"

// 打印棋盘
let printBoard (board: Board) =
    printfn "  ０ １ ２ ３ ４ ５ ６ ７ ８"
    for row = 0 to 9 do
        printf "%d " row
        for col = 0 to 8 do
            match Map.tryFind (row, col) board with
            | Some piece -> 
                Console.ForegroundColor <- if piece.Color = Red then ConsoleColor.Red else ConsoleColor.Blue
                printf "%s " (getPieceDisplay piece)
                Console.ResetColor()
            | None -> 
                if (row < 5 && row > 4) || (row = 4 && col <> 0 && col <> 8) then
                    printf "．"
                else
                    printf "＋ "
        printfn ""

// 检查移动是否有效（简化版）
let isValidMove (board: Board) fromPos toPos currentPlayer =
    match Map.tryFind fromPos board with
    | Some piece when piece.Color = currentPlayer ->
        // 这里应该实现各种棋子的具体移动规则
        // 为了简化，我们只检查目标位置是否为空或者是敌方棋子
        match Map.tryFind toPos board with
        | Some targetPiece -> targetPiece.Color <> currentPlayer
        | None -> true
    | _ -> false

// 移动棋子
let movePiece (board: Board) fromPos toPos =
    match Map.tryFind fromPos board with
    | Some piece ->
        let newBoard = board |> Map.remove fromPos |> Map.add toPos piece
        newBoard
    | None -> board

// 检查游戏是否结束
let isGameOver board =
    let hasGeneral color =
        Map.exists (fun _ piece -> piece.Type = General && piece.Color = color) board
    
    not (hasGeneral Red) || not (hasGeneral Blue)

// 主游戏循环
let rec gameLoop board currentPlayer =
    printBoard board
    printfn "%s方走棋，请输入移动（格式：行 列 行 列，例如：0 1 2 3）:" (if currentPlayer = Red then "红" else "黑(蓝)")
    
    let input = Console.ReadLine().Split()
    if input.Length <> 4 then
        printfn "输入格式错误，请重新输入！"
        gameLoop board currentPlayer
    else
        try
            let fromPos = (int input.[0], int input.[1])
            let toPos = (int input.[2], int input.[3])
            
            if isValidMove board fromPos toPos currentPlayer then
                let newBoard = movePiece board fromPos toPos
                
                if isGameOver newBoard then
                    printBoard newBoard
                    printfn "游戏结束！%s方胜利！" (if currentPlayer = Red then "红" else "黑")
                else
                    let nextPlayer = if currentPlayer = Red then Blue else Red
                    gameLoop newBoard nextPlayer
            else
                printfn "无效的移动，请重新输入！"
                gameLoop board currentPlayer
        with
        | _ -> 
            printfn "输入格式错误，请重新输入！"
            gameLoop board currentPlayer

// 开始游戏
[<EntryPoint>]
let main argv =
    printfn "欢迎来到中国象棋！"
    printfn "红方棋子：帅仕相马车炮兵"
    printfn "黑(蓝)方棋子：将士象马车炮卒"
    printfn "输入格式：起始行 起始列 目标行 目标列"
    printfn ""
    
    gameLoop initialBoard Red
    0