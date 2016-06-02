### Development

-   Things which were done as preprocessing steps: sampling, data
    cleansing, exploratory analysis, predictive modelling and some
    creative exploration.
-   The HC Corpora was used as data souce. Due to performance reason a
    sample of 10 percent of all data was drawn to build the
    language model. This sample was cleaned by transforming the text to
    lowercase, removing punctuation, links, extra white spaces, numbers
    and all kinds of special characters.
-   The next step was to creat tables out of the most frequent n-grams
    (1-gram, 2-gram, 3-gram, and 4-gram).
-   What is 'n-gram'? Source Wikipedia: In the fields of computational
    linguistics and probability, an n-gram is a contiguous sequence of n
    items from a given sequence of text or speech.
-   The n-grams were obtained by first cleaning the sampled text corpus
    and transform it in a next step to a term document matrix by using
    R´s tm-package. From this matrix uni-, bi-, and tri- term frequency
    matrices were processed and afterwards passed to data tables and
    saved in RData format.
-   As a next step the conditional probability for the last word given
    the first word(s) for each entry in the n-gram data tables have been
    computed and also stored in the data tables. The conditional
    probability was computed by applying a so called
    Kneser-Ney smoothing.

### Kneser-Ney Smoothing

The source used for the conditional Kneser-Ney probabilities for each
n-gram has been derived from the following document:

[Körner,
2013](https://west.uni-koblenz.de/sites/default/files/BachelorArbeit_MartinKoerner.pdf)

The Kneser-Ney smoothing procedure is a state of the art algorithm
widely used in NLP for next word prediction tasks. The Kneser-Ney
interpolated last word probabilities for the respective n-grams were
combined with a back-off algorithm IOT obtain also probabilities for
word combinations which were not seen in a higher order n-gram by
"backing off" to a lower order n-gram as follows:

### The Algorithm

-   It uses a 3-gram data table; the first two words of which are the
    last two words of the user input sentence were used to predict the
    next words.
-   The 3-gram data table is already sorted from highest to lowest
    interpolated Kneser-Ney conditional probabilities for the next word
    given the last two words of the user input.
-   The entered words are matched with all the 3-grams matching the last
    two words of the user input as the first two words of the 3-gram. If
    there are matches the top 5 word combinations according to their
    interpolated Kneser-Ney probabilities are returned to the user.
-   If no 3-gram is found, we back off to the 2-gram data table.
-   If no 2-gram word combinations match the user input a final back off
    to the unigram data table is applied.
