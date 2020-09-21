/**
 * Created by luism on 12-Jan-17.
 */

import React, {Component} from 'react';
import {
    StyleSheet,
    Text,
    View,
    Dimensions
} from 'react-native';
import Circle from './Circle';
import Space from './Space';
import Line from './Line';
import PlayerScore from './PlayerScore';
import BoardModel from './BoardModel';

const styles = require('./StyleSheet.js');
export default class Board extends Component {

    constructor(props) {
        super(props);
        let windowWidth = Dimensions.get("window").width;
        let windowHeight = Dimensions.get("window").height;
        let screenSize = windowWidth > windowHeight ? windowHeight : windowWidth;
        let numberOfLines = this.props.lines < this.props.columns ? this.props.columns : this.props.lines;
        this.lineSize = (screenSize * 0.79) / (numberOfLines * 6 + 1);
        this.state = {
            board: new BoardModel(this.props.lines, this.props.columns),
            boxes: new Array(this.props.lines * this.props.columns).fill(null),
            playerTurn: this.props.firstPlayer,
            playerOne: 0,
            playerTwo: 0,
            gameEnded: false,
        };
        this.keys = 0;

    }

    _onPress(line, x, y) {
        let boardModel = this.state.board;
        let boxes = this.state.boxes;
        let boxesBefore = boardModel.getNumeroCaixasFechadas(boxes, this.state.playerTurn);

        let array = boardModel.board;
        if (array[line][x][y] == void 0) {
            array[line][x][y] = this.state.playerTurn;

            let boxesAfter = boardModel.getNumeroCaixasFechadas(boxes, this.state.playerTurn);

            let change = boxesBefore != boxesAfter;

            this.setState({
                    board: boardModel,
                    boxes: boxes,
                    gameEnded: this.props.lines * this.props.columns == boxesAfter,
                    playerOne: (this.state.playerTurn == 1 && change == true) ? boxesAfter - this.state.playerTwo : this.state.playerOne,
                    playerTwo: (this.state.playerTurn == 2 && change == true) ? boxesAfter - this.state.playerOne : this.state.playerTwo,
                    playerTurn: change ? this.state.playerTurn : this.state.playerTurn == 1 ? 2 : 1,
                },
                () => {
                    if (this.state.gameEnded) {
                        this.props.navigator.push({
                            id: 'winner',
                            playerOne: this.props.playerOneName,
                            playerTwo: this.props.playerTwoName,
                            playerOneScore: this.state.playerOne,
                            playerTwoScore: this.state.playerTwo,
                        })
                    }
                });
        }

        /*if(this.props.vsMachine){
         require(['require-config'], function() {
         require(["jquery"], function($) {

         let data = {'tabuleiro': {
         'arcos': boardModel.board,
         'peca': this.state.playerTurn,
         'caixasJ1': this.state.playerOne,
         'caixasJ2': this.state.playerTwo
         }
         };

         $.ajax("/save-data", {
         data: JSON.stringify(data),
         dataType: "json",
         type: "POST"
         }).done(function (response) {
         alert(JSON.stringify(response));
         //$("#saved-data").text(JSON.stringify(response));
         });
         });
         });
         }*/
    }

    createBoardView() {
        let array = [];
        let arrayHorizontal = [];
        let arrayVertical = [];
        let i, j;
        for (i = 0; i < this.state.board.board[0].length; i++) {

            for (j = 0; j < this.state.board.board[0][i].length; j++) {
                array.push(<Circle key={this.keys++} componentSize={this.lineSize}/>);
                array.push(<Line key={this.keys++} isHorizontal={true} x={i} y={j} board={this.state.board.board}
                                 onPressAction={this._onPress.bind(this)} componentSize={this.lineSize}/>);
            }
            array.push(<Circle key={this.keys++} componentSize={this.lineSize}/>);

            arrayHorizontal.push(<View style={styles.row} key={this.keys++}>{array}</View>);

            array = [];

            if (i < this.state.board.board[0].length - 1) {
                for (j = 0; j < this.state.board.board[1].length - 1; j++) {
                    array.push(<Line key={this.keys++} isHorizontal={false} x={j} y={i} board={this.state.board.board}
                                     onPressAction={this._onPress.bind(this)} componentSize={this.lineSize}/>);
                    array.push(<Space key={this.keys++} componentSize={this.lineSize}
                                      isClosed={this.state.boxes[i*(this.state.board.board[0].length-1)+j]}/>);
                }
                array.push(<Line key={this.keys++} isHorizontal={false} x={this.state.board.board[1].length-1} y={i}
                                 board={this.state.board.board} onPressAction={this._onPress.bind(this)}
                                 componentSize={this.lineSize}/>);
                arrayVertical.push(<View style={styles.row} key={this.keys++}>{array}</View>);
                array = [];
            }

        }

        for (i = 0; i < arrayVertical.length; i++) {
            array.push(arrayHorizontal[i]);
            array.push(arrayVertical[i]);
        }
        array.push(arrayHorizontal[arrayHorizontal.length - 1]);
        this.keys = 0;
        return array;
    }

    render() {

        let board = this.createBoardView();

        return (
            <View style={styles.container}>
                <View style={{flex:1, flexDirection: 'row'}}>
                    <PlayerScore playerName={this.props.playerOneName} peca={1} playerTurn={this.state.playerTurn}
                                 score={this.state.playerOne} stopAnimation={this.state.gameEnded}/>
                    <PlayerScore playerName={this.props.playerTwoName} peca={2} playerTurn={this.state.playerTurn}
                                 score={this.state.playerTwo} stopAnimation={this.state.gameEnded}/>
                </View>
                <View style={{flex: 2}}>
                    {board}
                </View>
            </View>
        );
    }
}