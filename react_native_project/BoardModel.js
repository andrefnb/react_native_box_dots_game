/**
 * Created by luism on 13-Jan-17.
 */

export default class BoardModel{

    constructor(lines, columns){
        this.board = this.createBoard(lines, columns);
    }

    createBoard(lines, columns){
        let array = [[],[]];
        let i;
        for(i = 0; i<=lines; i++){
            array[0].push(this.lineEmpty(columns, true));
        }
        for(i = 0; i<=columns; i++){
            array[1].push(this.lineEmpty(lines, false));
        }
        return array;
    }

    lineEmpty(number,isLine){
        let array = [];
        let i;
        for(i=0; i<number; i++){
            if(isLine){
                array.push(null);
            }else{
                array.push(null);
            }
        }
        return array;
    }

    getNumeroCaixasFechadas(boxesArray, player){
        let lines = this.board[0];
        let columns = this.board[1];
        let boxes = 0;

        for(let i = 0; i< lines.length-1; i++){
            for(let j = 0; j<lines[i].length; j++){
                if(lines[i][j] != void 0 && lines[i+1][j] != void 0 && columns[j][i] != void 0 && columns[j+1][i] != void 0){
                    if(boxesArray[i*(lines.length-1)+j] == void 0){
                        boxesArray[i*(lines.length-1)+j] = player;
                    }
                    boxes++;
                }
            }
        }

        return boxes;
    }
}