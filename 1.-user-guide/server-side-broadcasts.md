---
description: This page contains the information for the server side broadcast action codes
---

# Server Side Broadcasts

For the following table we'll assume the player who creates the game is **client1**, and the player who joins is **client2**

<table data-full-width="true"><thead><tr><th width="142.33333333333331">Action Code</th><th width="375">Action</th><th>Broadcasted</th></tr></thead><tbody><tr><td>200</td><td>Join game lobby</td><td>200|client1|client1_nickname_example|client1_avatar_link_example</td></tr><tr><td>201</td><td>Leave game lobby</td><td>201|client2</td></tr><tr><td>202</td><td>Join Ongoing game</td><td>202|client2</td></tr><tr><td>203</td><td>Leave Ongoing game</td><td>203|client2</td></tr><tr><td>204</td><td>Changes avatar/nickname</td><td>204|client1|updated_client1_nickname_example|updated_client1_avatar_link_example</td></tr><tr><td>205</td><td>Starts game, provides starting zone index</td><td>205|4</td></tr><tr><td>206</td><td>Host Leaves and a new one is assigned</td><td>206|client1|client2</td></tr><tr><td>207</td><td>Host disconnects and a new one is assigned</td><td>207|client1|client2</td></tr></tbody></table>
