/**
 * Created by luism on 16-Jan-17.
 */

import React, { Component } from 'react';
import {
    StyleSheet,
    Text,
    View,
    Button,
    TextInput,
    Navigator,
    Dimensions,
    Image,
    Slider
} from 'react-native';
import Board from './Board';

const styles = require('./StyleSheet.js');
class Main extends Component {
    render() {
        return (
            <View style={styles.container}>
                <View style={{flex:1, justifyContent: 'center', alignItems: 'center',}}>
                    <Image source={require('./Logo.png')} style={{width: 300, height: 300, margin: 30}}/>
                    <View style={{width: Dimensions.get("window").width*0.85}}>
                        <Button
                            onPress={this.navSecond.bind(this)}
                            title="Press Start"/>
                    </View>
                </View>
            </View>
        );
    }

    navSecond() {
        this.props.navigator.pop();
        this.props.navigator.push({
            id: 'type'
        });

    }
}

class TypeOfGame extends Component {

    render() {
        return (
            <View style={styles.container}>
                <View style={{flex: 1, flexDirection: 'column', justifyContent: 'space-between', alignItems: 'center'
                , marginBottom: Dimensions.get('window').height*0.3}}>
                    <Text style={{fontSize: 30, fontWeight: 'bold', textAlign: 'center', marginTop: 40}}>Choose your type of Game:</Text>
                    <Button title="Human vs Human" onPress={() => this.navSecond(false)} raised='true'/>
                    <Button title="Human vs Machine" onPress={() => this.navSecond(true)}/>
                </View>
            </View>
        );
    }

    navSecond(vsMachine) {
        this.props.navigator.push({
            id: 'settings',
            vsMachine: vsMachine,
        })
    }
}

class Settings extends Component{
    constructor(props){
        super(props);
        this.state = {
            playerOneName: "",
            playerTwoName: "",
            firstPlayer: 1,
            lines: 1,
            columns: 1,
        };
    }

    render(){
    //alterar a forma que o utilizador introduz as dimensões do tabuleiro (usando o slider)
        return(
            <View style={{flex:1, flexDirection: 'column', justifyContent: 'space-between' ,alignItems: 'center', margin: 80 || 0 || 80 || 0}}>

                <View style={{borderWidth: 0.5, borderColor: '#0f0f0f', padding: 20}}>
                    <Text>Player One Name: </Text>
                    <TextInput autoCorrect={false} autoCapitalize='words' onChangeText={(text) => this.setState({ playerOneName: text})}/>
                </View>
                <View style={{borderWidth: 0.5, borderColor: '#0f0f0f', padding: 20}}>
                    <Text>Player Two Name: </Text>
                    <TextInput autoCorrect={false} autoCapitalize='words' onChangeText={(text) => this.setState({ playerTwoName: text})}/>
                </View>


                <View>
                    <Text>Number of Lines: {this.state.lines}</Text>
                    <Slider minimumValue={1} maximumValue={7} step={1} style={{width: 200}} onValueChange={(value) => this.setState({ lines: value})} />
                </View>
                <View>
                    <Text>Number of Columns: {this.state.columns}</Text>
                    <Slider minimumValue={1} maximumValue={7} step={1} style={{width: 200}} onValueChange={(value) => this.setState({ columns: value})} />
                </View>

                <Button
                    onPress={this.navSecond.bind(this)}
                    title="Start"
                    color="#841584"/>
            </View>
        )
    }

    navSecond() {
        this.props.navigator.push({
            id: 'board',
            vsMachine: this.props.vsMachine,
            playerOne: this.state.playerOneName,
            playerTwo: this.state.playerTwoName,
            turn: this.state.firstPlayer,
            lines: parseInt(this.state.lines),
            columns: parseInt(this.state.columns),
        })
    }
}

class Main2 extends Component {
    render() {
        return (
            <View style={styles.container}>
                <Board navigator={this.props.navigator} lines={this.props.lines} columns={this.props.columns} vsMachine={this.props.vsMachine} firstPlayer={this.props.turn} playerOneName={this.props.playerOne} playerTwoName={this.props.playerTwo} />
            </View>
        );
    }
}

class Winner extends Component {

    winnerMessage(playerName, playerScore){
        return 'The winner was ' + playerName + ' with ' + playerScore + ' boxes closed';
    }

    render() {
        let winner = this.props.playerOneScore > this.props.playerTwoScore ? 1 : this.props.playerOneScore < this.props.playerTwoScore ? 2 : 0;

        let winnerMessage = winner == 0 ? 'It was a draw, both players closed ' + this.props.playerOneScore + ' boxes'
            : winner == 1 ? this.winnerMessage(this.props.playerOne, this.props.playerOneScore)
            : this.winnerMessage(this.props.playerTwo, this.props.playerTwoScore);

        return (
            <View style={styles.container}>
                <Text>{winnerMessage}</Text>
            </View>
        );
    }
}

export default class Navigation extends Component {
    render() {
        return (
            <Navigator
                initialRoute={{id: 'first'}}
                renderScene={this.navigatorRenderScene}/>
        );
    }

    navigatorRenderScene(route, navigator) {
        switch (route.id) {
            case 'first':
                return (<Main navigator={navigator}/>);
            case 'board':
                return (<Main2 navigator={navigator} vsMachine={route.vsMachine} playerOne={route.playerOne} playerTwo={route.playerTwo} turn={route.turn} lines={route.lines} columns={route.columns} />);
            case 'settings':
                return (<Settings navigator={navigator} vsMachine={route.vsMachine}/>);
            case 'type':
                return (<TypeOfGame navigator={navigator}/>);
            case 'winner':
                return(<Winner navigator={navigator} playerOne={route.playerOne} playerTwo={route.playerTwo} playerOneScore={route.playerOneScore} playerTwoScore={route.playerTwoScore}/>);
        }
    }
}