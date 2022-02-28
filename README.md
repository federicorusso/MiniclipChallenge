<h1>Backend Developer - Miniclip Challenge</h1>
<h2>Author: Federico Russo (@federico__russo on Github)</h2>

This repository is managed so to provide version control over the files implemented for the Miniclip Backend Developer Challenge.<br>

Last update: 01/03/22
How to launch the app:

<ol>
  <li>cd [...]/fr_miniclip_challenge</li>
  <li>Open a linux/windows shell</li>
  <li>rebar3 shell - This command will provide information about the Port being used and the maximum concurrent connections managed</li>
  <li>Open 1+ linux/windows shell(s) and connect to the port, e.g. telnet localhost 8888</li>
  <li>Login via your username</li>
</ol>

**********************************************************************************************

<ul>
<li>Mandatory tasks:</li>
<ol>
  <li>The server application must accept and manage multiple TPC/IP client connections
<br><u>Status</u>: Completed 28/02/22</li>
  <li>A user must be able to create a room, as well as list, join (enter) and leave (exit) other created rooms
<br><u>Status</u>: In progress (40%)</li>
  <li>A user must be able to send a message to all the users in a specific room
<br><u>Status</u>: In progress (10%)</li>
</ol>
<br>
<li>Optional tasks:</li>
<ol>
  <li>A user should be able to send a message to a single user
<br><u>Status</u>: In progress (10%)</li>
  <li>A user should be able to invite other users to join them in a private room</li>
  <li>The network layer should be well separated from the chat server layer</li>
</ol>
<br>
<li>Extra tasks:</li>
<ol>
  <li>Messages and the full message history between clients and server are mediated through Google Protocol Buffer</li>
  <li>The chat messages are stored on an AWS Redis Database
</li>
</ol>
</ul>

Challenge start date: 	February 10th, 2022<br>
Challenge end date: 	February 24th, 2022<br>
<br>
Erlang/OTP version used: 22.3
Rebar3 version used: 3.18.0