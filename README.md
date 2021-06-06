# Snake Game
This was a University Project and implemented in Assembly Language. The rquirement for this project were as follows
* A snake that automatically moves left, right, up and down on the screen.
* Initial size of snake is 20 characters with head of the snake represented by a different character as compared to its body.
* The arrow keys are used to determine the direction of snake.
* If the snake’s head touches the border of screen the players loses one life. Initially the player has three lives. After losing all three lives the game is over.
* If the snake eats a fruit the size of snake is increased by four characters. 
* If the snake does not eat the fruit, the fruit remains on the screen.
* After the fruit is eaten by the snake, the next fruit immediately appears on a random location on the screen.
* Maximum size of snake can be 240 characters.
* If the snake does not reach the maximum size in 4 minutes one life of player will end.
* If the snake reaches the maximum size in 4 minutes the player earns some points and game ends with an appropriate message.
* After every 20 seconds the speed of snake is twice as the previous speed.
* Show on display the remaining and total lives.
* Show on display the time remaining and when the time is reset. 
* If the snake touches itself one life of player will end.

Extra Functionalities Implemented
*	Sound
*	Colors
*	Different fruit shapes
*	Number of stages
*	Interface improvised e.g. apart from the boundary wall create more hurdles for the snake OR apart from fruits he eats, display some dangerous fruits which if eaten will reduce its size or one life.

## How To Run?
This was developed DoSBox and nasm assembler. you can run following commands to run it
```
> nasm surpant.asm -o surpant.com
> surpant.com
```
## Sample Screens
```
This is Home Page
```
![image](https://user-images.githubusercontent.com/54348709/120939167-17684880-c730-11eb-8ace-8adc7606fb51.png)

```
This is game UI
```
![image](https://user-images.githubusercontent.com/54348709/120939190-38309e00-c730-11eb-8e82-365ebdca952e.png)

```
This is sample fruit and hurdle UI
```
![image](https://user-images.githubusercontent.com/54348709/120939210-56969980-c730-11eb-8fa5-a64f36e70dd0.png)

```
This is Game Over Page
```
![image](https://user-images.githubusercontent.com/54348709/120939271-bbea8a80-c730-11eb-8641-d8e4cfe4e82b.png)
