/**
 * Created by luism on 12-Jan-17.
 */

import React, {Component} from 'react';
import {
    StyleSheet,
    TouchableOpacity,
    View
} from 'react-native';

export default class Space extends Component {
    constructor(props){
        super(props);
        this.styles = {
            width: this.props.componentSize*5,
            height: this.props.componentSize*5,
        };
    }


    render() {
        return (
            <View style={[this.styles, {backgroundColor : this.props.isClosed == void 0 ? 'transparent' : this.props.isClosed == 1 ? '#900' : '#009'}]} />
        );
    }
}