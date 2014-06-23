FrequentPatterns
================

This is a project focused on building out an implementation of FPTree and FP-growth in scala.

It is an implementation of the approach described in

>Han, Jiawei, et al. “Mining frequent patterns without candidate generation: A frequent-pattern tree approach.” Data mining and knowledge discovery 8.1 (2004): 53-87.

Use Case
--------

This project is a first part of a larger project.  I intend it to first serve as an example and hopefully I will have time to clean up documentation and describe it in more detail over time.
The following is a good survey of the standard approaches.

>Hipp, Jochen, Ulrich Güntzer, and Gholamreza Nakhaeizadeh. “Algorithms for association rule mining—a general survey and comparison.” ACM sigkdd explorations newsletter 2.1 (2000): 58-64.

The basic use case is a simple question: “I have a dataset; what are the frequent patterns contained within it?”

Let’s define a few terms in order to proceed:

* A *Transaction Database* is a list of *Transactions*
* A *Transaction* is a record that contains items drawn from some item collection.
    * The fact that the universe of items could be huge is what makes this problem interesting: the number of possible patterns is exponential in the number of possible items.
* An *Item Set* is a subset of items pulled from a *Transaction* (classic example: {bread, butter, pack of kools, diet rite, diapers})
* The *Support* of an *Item Set* is the number of *Transactions* in the *Transaction Database* that contain it.  Sometimes this is given as a percentage of the size of the database.

### Association Rule Mining

put in a write-up on Association rule mining.

### Contrived Example

As stated above the general procedure for pattern mining is two fold

1. Find all patterns that meet some predetermined support threshold.
2. Find all the patterns within those that meet some minimum *interestingness* threshold.  The definition of *interestingness* is a tricky one.

Recently I was toying with the Enron dataset for no good reason.  I found myself looking at the email headers and I asked a natural question.  “Which headers can I count on having if I try to process them?”
This is how I answered the question (the functions I use are in a different project, but they don’t really matter here):

Prepare a sample of the data to investigate

    val files = getFiles(new File(“/data/datasets/enron_mail_20110402/maildir”))
    val lines: Stream[Option[List[String]]] = files.take(10000).map(getLinesFromFile(_))
    // the files might throw exceptions so the flatten on the next line is there just to ignore them
    val headers = lines.flatten.map(getHeader(_))
    val headerFields = headers.map(_.keys.toList).toList
    val fp = FrequentPatterns(headerFields) //note lack of support threshold here.  I am still investigating

Decide on my threshold: “Let’s find all patterns that occur in EVERY record.”

    val appearsInAll = fp.filter(_._2 == headerFields.length)

Now I still have a pretty big list.  Recall `appearsInAll` contains *all* the patterns: that is all the 1-itemsets, 2-itemsets, etc.  So, I need to apply some *interestingness* threshold.
I decide to define that as “is the pattern long?”

    scala> appearsInAll.map(_._1.length).max
    res18: Int = 10

    appearsInAll.filter(_._1.length==10)

This results in one pattern: `List((List(X-Folder, From, X-Origin, Date, Content-Type, Message-ID, Content-Transfer-Encoding, X-From, X-FileName, Mime-Version),9986))`

That is not very satisfying!  I cannot reliably get a *To:* line on all of my messages.

Let’s change our support threshold.  Let’s be less greedy and find patterns that appear in 90% of the records

    val appearsInMost = fp.filter(_._2 >= headerFields.length*.9)
    appearsInMost.map(_._1.length).max // 13
    appearsInMost.filter(_._1.length==13)

Much better! `List((List(X-Folder, From, X-Origin, Date, Content-Type, Message-ID, Content-Transfer-Encoding, X-From, X-FileName, Mime-Version, Subject, X-To, To),9472))`




