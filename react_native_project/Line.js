/**
 * Created by luism on 12-Jan-17.
 */

import React, {Component} from 'react';
import {
    StyleSheet,
    TouchableOpacity,
    View
} from 'react-native';

const styles = require('./StyleSheet.js');

class Line extends Component {

    constructor(props){
        super(props);
        this.x = this.props.x;
        this.y = this.props.y;
    }

    setLineStyle(){
        return this.line(this.props.componentSize, this.props.isHorizontal);
    }

    line(size, isHorizontal) {
        return {
            width: (isHorizontal==true) ? size*5 : size,
            height: (isHorizontal==true) ? size : size*5,
        };
    };

    render() {
        let line = this.props.isHorizontal ? 0 : 1;
        let currentState = this.props.board[line][this.x][this.y];
        return (
            <TouchableOpacity
                onPress={() => this.props.onPressAction(line, this.x, this.y) }
                style={currentState == void 0 ? [this.setLineStyle(), styles.blank] : currentState == 1 ? [this.setLineStyle(), styles.playerOne] : [this.setLineStyle(), styles.playerTwo]}
                hitSlop={this.props.hitSlop}>
            </TouchableOpacity>
        );
    }
}

export default Line;