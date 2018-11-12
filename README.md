# bittorrent

## TODO


## Datastructures

This bittorrent client is implemented as a state machine:

    StateMachine

The state machine is pure and us updated by messages:

    update :: Message -> StateMachine -> StateMachine

The state machine can return >= 0 actions:
    
    update :: Message -> StateMachine -> ([Action], StateMachine)

An action is description of some stateful action that needs to be executed. It
is the interface between the pure state machine and the effectful IO layer:

    eval :: Action -> ActionInterpreter ()




## ACTION

`Action`s are instructions to the interpreter to perform some effectful IO:

    - SendMessage
    - ReceiveMessage
