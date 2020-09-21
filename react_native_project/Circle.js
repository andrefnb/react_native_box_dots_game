/**
 * Created by luism on 12-Jan-17.
 */

import React, {Component} from 'react';
import {
    StyleSheet,
    TouchableOpacity,
    View
} from 'react-native';

export default class Circle extends Component {
    constructor(props){
        super(props);
        this.styles = {
                width: this.props.componentSize,
                height: this.props.componentSize,
                borderRadius: this.props.componentSize/2,
                backgroundColor: 'black'
            };
    }

    render() {
        return (
            <View style={this.styles} />
        );
    }
}