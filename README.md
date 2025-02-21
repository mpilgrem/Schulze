# Schulze

A Haskell package to apply the Schulze method to the result of a vote.

Command `Schulze <path_to_vote_result>`. If no vote result is specified,
`exampleVote.yaml` is used, which is based on the Wikipedia example for the
method.

A vote result is set out in a YAML file, with the format:
~~~yaml
candidates:
  A: "Identity of candidate A"
  B: "Identity of candidate B"
  C: "Identity of candidate C"
  D: "Identity of candidate D"
  E: "Identity of candidate E"

votes: # A dictionary of the votes cast
  "Identity of a vote":
    count: 1 # Optional frequency of the vote (default is 1)
    A: 1 # Pairs of candidate reference and numercial rank
    C: 2
    B: 3 # Ties are allowed
    E: 3 # Ties are allowed
    D: 5
~~~
