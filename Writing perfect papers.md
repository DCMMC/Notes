## Writing perfect papers

> A note of [lecture](https://bilibili.com/video/BV18v411n7mr) from Prof. Baochun Li

Top-down:

### The story

The important entry point of a perfect paper.

Essential elements:

* The problem (first part in the story)

> 1. doesn't to be "trendy" (will be hard)
> 2. you really enjoy working on
> 3. trend will not last forever

* What's new (the contribution)?

> 1. First briefly introduce the state-of-the-art, then write about what is new
> 2. If the problem if different from previous, provide justfications of the important and necessary of the difference
> 3. Highlights the contribution

* How the solution is better (advancing the state-of-the-art)?

> 1. stronger theoretical properties or better experimental results
> 2. effectiveness and efficiency

### The (hard) work

interaction between read and write (you cannot only read 2 months without writing)

* Read

> 1. Become an expert on the problem
> 2. Start from a single paper (e.g. highly cited seminal paper), then do an **expanded-ring search** (BFS, e.g. papers cited, papers authored by same researchers, do not depend (only) on search engine)
> 3. Narrow to tiny slice of papers of the **particular** problem, read **quickly** first (e.g., 10 min), read more **carefully** if necessary later.

* Write

> 1. Writing every day (a or two paragraphs per day, and collecting them to a paper after one months)
> 2. Write a (incomplete) paper (e.g., related work, challenging, why the problem is important w/o copying and pasting from the other papers, what needs to be fixed, write a survey paper) and then do research (implementation of the method)
> 3. Writing is the best way to force yourself to think clearly and be focused to create something (e.g., crystallize what you donot quite understand yet, open dialogue with others) rather than consuming when reading
> 4. Consider the readers: write sentences as clear as possible, every statement need evidences (experimental results or cited reference), use symbols or terms after clear definition

### The art

Pay attention to the title (2 lines, 10,000 readers), abstract (10 lines, 1,000 readers), intro (100 lines, 100 readers), and flow of ideas.

* title: attracts a reader to read the abstract, as precise and simple (but not general) as possible, reflect the essence of the new idea
* abstract: conveys essential information  (i.e., background, what the problem is, what's new, how the solution is better) about the paper
* introduction: impress the reader (self-contained: even for the reader who only read the intro) with an exciting and clear introduction; do not spend too much space on the background and related work; do not make work sound more original than it really is (be honest);

> Typical structure:
>
> (1) general overview of the research field (2-3 sentences) to warm up the reader;
>
> (2) problem statement (in the second paragraph): clear description (do not mix the basic idea with unnecessary details; smooth transition) of the challenges and the problem at a high level; it's fine to include an intuituive solution to the problem; the existing solutions; (1-2 paragraphs)
>
> (3) Limitations of existing solutions that **motivate** this paper (2-3 paragraphs)
>
> (4) Main idea of proposed solution main idea (1-2 paragraphs)
>
> (5) (Contribution) The **original** highlights of the proposed solution (1-2 paragraphs): why this proposed solution different from and better than existing ones; 1-2 most impressive highlights; make the originiality of the paper crystal clear and stand out;The list of original contributions **drives** the entire paper;
>
> Tips:
>
> (1) Use an (simple) example (to help readers to understand the solution)
>
> (2) Show in tables (e.g., highlights compared with directly related work)
>
> (3) Use well-designed figures (as many as possible)

#### Flow of ideas

each paragraph contains one complete idea

Related work:

* place it after the introdution or before the conclusio are both fine, depending on the flow of ideas.

Experiements:

* Some negative or preliminary experiments (before the section of methonology) may be important for motivating the main idea in the paper. Example flow: initial results => idea to improve => more results to show better performance.

* You can even interleave experiments within the descriptions of the idea if necessary.

### The detail

* VCS such as git help you to track all histories and collaborate with others.

* Eliminate all spelling mistakes through a spell checker
* Use transitions across boundaries
* Do not use long sentences (more than one comma)
* Do not be too colloquial (a large number of instead of a lot of, large or substantial instead of big)
* Do not too formal (e.g., endeavour, ascribe)
* Do not too emotional (e.g., happy, crazy, fantastic, suicide)
* countable and uncoutable nouns
* Use LaTeX
* Use vector-based figures (e.g., draw.io)
* Proofread the bibliography and make it consistent (prevent citing arXiv if the manuscript is published)

## Writing good papers fast

deadline is important

* Set your deadline realisitically (write a good paper in [2, 6] months)
* Design rough timeline with milestones
* Do not be procrastination
* Choose a problem with the right size
* Estimate the amount of time to reproduce the code of a paper without public code
* Read paper with source code (if available) which contains secrets
* The problem need fit into the scope of the conference or journal
* Many conferences have their favoured research topics