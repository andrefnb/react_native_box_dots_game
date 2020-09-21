/**
 * Created by luism on 12-Jan-17.
 */

import { StyleSheet, Dimensions } from 'react-native';

module.exports = StyleSheet.create({
    container: {
        flex: 1,
        justifyContent: 'center',
        alignItems: 'center',
        backgroundColor: '#F5FCFF',
    },

    row: {
        flexDirection: 'row'
    },

    playerOne: {
        backgroundColor: 'red'
    },

    playerTwo: {
        backgroundColor: 'blue'
    },

    blank: {
        backgroundColor: 'transparent'
    },
});