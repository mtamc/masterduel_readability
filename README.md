This is a personal playground built on top of
<https://github.com/RndUser0/YGOMD-Improve_Card_Text_Readibility>

### How to generate the modded files

Untested on other machines, you might have some dependency issues. Probably Linux only.

When the game has updated and you haven't installed the mod, run
"./get_original_files.sh".

Then to generate the release files, run "./make_release_files.sh"


### Current features

- Numbered card effects separated by single newlines
- Certain verbose properties are changed to tags written at the start of an
    effect as "TAG1 | TAG2 | ...": OPT (soft once per turn), HOPT (hard once per
    turn), QUICK (quick effect), X PHASE/YOUR X PHASE/OPP'S X PHASE (effect that
    can only be activated during a certain phase),
- Effect activation costs (clauses before a semicolon) are bolded. Conditions
    (clauses before a colon) are currently unformatted, because I tried italics
    and it looked too noisy.
- "Graveyard" is always abbreviated to "GY"
- keywords are uppercased: X SUMMON(S/ING/ED...), DESTROY(...), NEGATE, DISCARD,
    QUICK EFFECT, HANDsdeck, BANISH(...), EXCAVATE(...), DRAW(...), PIERCING
- Useless text "DESTROYED by battle or card effect(s)" shortened to "DESTROYED"
- Certain verbose actions/properties are reworded:

| original | reworded |
| --- | --- |
| Add X from your deck to your hand | SEARCH X |
| Add X from your deck or GY to your hand | SEARCH X (from DECK or GY) |
| Return X (from the field) to the hand | BOUNCE x |
| Returned to the hand | BOUNCED |
| Send X from the top of your deck to the GY | MILL X |
| If X attacks a defense position monster, inflict piercing battle damage | X is PIERCING |
| If X attacks a Defense Position monster whose DEF is lower than the ATK of the equipped monster, inflict the difference as Battle Damage | X is PIERCING |

Note: Sometimes, certain effects are "unregistered" in masterduel (they have no in-duel highlight). When those are one-sentence (or in certain cases 2), they are processed by this mod, but when there are multiple sentences they are mostly unprocessed as the logic to separate them into discrete effects is not implemented yet (and probably would be fairly complex to implement).

Those unregistered effects still have the rewords in the above table, keyword
uppercasing, activation cost bolding, "Graveyard" replaced to "GY", and
"destroyed by battle or card effect(s)" shortened to "destroyed". They don't
have effect tags (QUICK, X PHASE, OPT, HOPT).

### Planned

- Accepting suggestions
