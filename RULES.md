The *shield* is a list of tones. Every time you tap, a new cosine tone is added to the
shield with\:
* Amplitude 1;
* Wavelength equal to the time since your last tap;
* (which means a frequency of the reciprocal of that interval);
* Offset equal to the game time at which your last tap occurred.

Each tone in the shield decays exponentially, multiplying by k^f each tick (where f is
the wave's frequency and k is a global constant less than one). This means that faster
waves decay faster.

When a tone's amplitude becomes very small (say when it is less than Q), that tone is
removed from the shield.

A *bolt* flies toward the player with velocity given by
> engine^2 * shield + V
where `engine` is a tone associated with the bolt, `V` is a constant, and `shield` is
the sum of all the tones in the shield. If it hits the player, game over.

The *game loop* is as follows:
1. Decay all tones in the shield.
2. If the player tapped, a new tone is added to the shield.
3. All bolts on screen move according to their velocity.
4. If a bolt collides with the player, trigger loss condition.
5. If a bolt is more than R distance away from the player, destroy it.
6. Maybe spawn one or more new bolts.
