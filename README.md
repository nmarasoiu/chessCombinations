# chessPieces
Given 2 Kings, 2 Queens, 2 Bishops and 2 Knights (all same colour), there are about 17.5 million ways to arrange the pieces so they do not attack each other on a 7 by 7 table.
solutions(Input(Table(7, 7), Map(King -> 2, Queen -> 2, Bishop -> 2, Knight -> 2)) -> a Monix Observable with 17M results.
Given 2 Rooks as well, the number of arrangements go down to about 300K.
The problem is very interesting because we want to be as fast as possible, that also means using multiple threads and be thread safe, take care of memory, use streaming if possible, to avoid accumulating in memory millions of results, and also allow consumers to already start consuming results before all of them are generated. A lot of profiling, memory statistics and so on, using JProfiler, helped me optimize the code. Deep enough use of Functional Programming and Reactive Programming, more practically using Monix, as Reactive Streams implementation, also add to the interesting points.
