/**
 * Created by luism on 16-Jan-17.
 */
import React, {Component} from 'react';
import {
    StyleSheet,
    Text,
    View
} from 'react-native';
const styles = require('./StyleSheet.js');
export default class PlayerScore extends Component{

    constructor(props){
        super(props);
        this.playerName = this.props.playerName;
        this.peca = this.props.peca;
        this.playerStyle = this.peca==1 ? styles.playerOne : styles.playerTwo;
    }

    render() {
        return (
            <View style={styles.container}>
                <Text style={this.props.playerTurn == this.peca ? this.playerStyle  : {backgroundColor: 'transparent'}}>Player{this.peca}: {this.playerName}</Text>
                <Text>Score: {this.props.score}</Text>
            </View>
        );
    }
}