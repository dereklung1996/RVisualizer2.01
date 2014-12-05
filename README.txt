 # # # # # # # # # # # # # # # # # # # # # # # # #  # # # # # # # # # # # # # # # # # # # # # #
/\  _`\ /\ \/\ \/\__  _\/\  _`\ /\ \/\ \/\  _  \/\ \   /\__  _\/\_____  \ /\  _`\ /\  _`\     
\ \ \L\ \ \ \ \ \/_/\ \/\ \,\L\_\ \ \ \ \ \ \L\ \ \ \  \/_/\ \/\/____//'/'\ \ \L\_\ \ \L\ \   
 \ \ ,  /\ \ \ \ \ \ \ \ \/_\__ \\ \ \ \ \ \  __ \ \ \  __\ \ \     //'/'  \ \  _\L\ \ ,  /   
  \ \ \\ \\ \ \_/ \ \_\ \__/\ \L\ \ \ \_\ \ \ \/\ \ \ \L\ \\_\ \__ //'/'___ \ \ \L\ \ \ \\ \  
   \ \_\ \_\ `\___/ /\_____\ `\____\ \_____\ \_\ \_\ \____//\_____\/\_______\\ \____/\ \_\ \_\
    \/_/\/ /`\/__/  \/_____/\/_____/\/_____/\/_/\/_/\/___/ \/_____/\/_______/ \/___/  \/_/\/ /
 # # # # # # # # # # # # # # # # # # # # # # # # #  # # # # # # # # # # # # # # # # # # # # # #

Rvisualizer is a media player that has multiple visuals unique to the song.
The visualizer is able to cycle through songs and play them. Like any media player, it 
can pause and play the song at any time with a slider. Volume can also be changed 
using a slider.

FEATURES:
Play button
Pause Button
Next Song
Previous Song
Mute
Volume Slider
Song Position Slider
5 Different Visual Tabs
Return to Menu Button

INSTRUCTIONS
To start, press 'visualizer' on the main menu.
To play the music all you have to do is click play with the mouse.
Song position and volume slider are draggable. 
Pressing 'next' and 'previous' will change the currently playing song.
The tabs at the top labeled 1-5 change the style of the visualization currently playing.
Click the back button on the top left to go back to the main menu.
Click the ??? button on the main menu for a secret.

WARNING
When cycling through the songs make sure you are at the beginning of the song or the program will crash. 
(Technical Note) We believe this is  their are two asynchronous channels in the program. Simply put, when the program switches 
from a long song to a short song, it attempts to find a sample at a frame that does not exist. This is because the
asynchronous channels either takes too long to reset the network's counter to zero, or possibly because the buffer limit
of the channel has been reached, despite it being set to #f, which should remove the limit.

ADDING SONGS
There is no user interface to add songs

To add songs manually you must place your 16bit WAV files into the songs folder
Then you must add the path of the song as a definition in DrRacket and add it to the list
of songs and song names.
