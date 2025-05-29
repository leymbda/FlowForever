namespace FlowForever.Domain

type Game = {
    Board: Board
    CurrentlySelectedIndex: int option
}

module Game =
    let create board =
        {
            Board = board
            CurrentlySelectedIndex = None
        }

    //let interact coordinate game =
    //    match game.CurrentlySelectedIndex, Board.tryGetCell coordinate game with
    //    | None ->
    //    game.Board |> Board.trySetCell 
