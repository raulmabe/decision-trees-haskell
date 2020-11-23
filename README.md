# Decision Trees in Haskell

## Description

This program generates an optimal decision tree based on this [Mushroom dataset](https://archive.ics.uci.edu/ml/datasets/Mushroom). It also predicts if a given mushroom may be edible or poisonous based on the decision tree generated.

## Usage

Clone the repository and generate the executable

    ghc dts.hs

Then execute the generated executable

    ./dts

## Example

    $ ./dts
    --- PRETTY DECISION TREE ---
    odor
      almond
        edible
      anise
        edible
      creosote
        poisonous
      fishy
        poisonous
      foul
        poisonous
      musty
        poisonous
      none
        spore-print-color
          black
            edible
          brown
            edible
          buff
            edible
          chocolate
            edible
          green
            poisonous
          orange
            edible
          white
            habitat
              grasses
                edible
              leaves
                cap-color
                  brown
                    edible
                  cinnamon
                    edible
                  white
                    poisonous
                  yellow
                    poisonous

              paths
                edible
              waste
                edible
              woods
                gill-size
                  broad
                    edible
                  narrow
                    poisonous


          yellow
            edible

      pungent
        poisonous
      spicy
        poisonous

    --- CLASSIFICATION ---
    Which odor?
    musty
    Prediction: poisonous

## License

MIT License
