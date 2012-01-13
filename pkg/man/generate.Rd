\name{generate}
\alias{generate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Generate sequences using a PST
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
generate(object, l, n, s1, p1, method, L, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	A PST, that is an object of class \code{\link{PSTf}} as returned by \code{\link{pstree}} or \code{\link{prune}}.
}
  \item{l}{
	Integer: Length of the sequence(s) to generate
}
  \item{n}{
	Integer: Number of the sequence(s) to generate
}
  \item{s1}{
	Character vector: The first state in the sequences. The length of the vector should equal \code{n}. If specified, the first state in the sequence(s) is not randomly generated but taken from \code{s1}. 
}
  \item{p1}{
	Numeric vector: An optional probability vector for generating the first position state in the sequence(s). If specified, the first state in the sequence(s) is randomly generated using the probability distribution in \code{p1} instead of the probability distribution taken fron the root node of \code{object}. 
}
  \item{method}{
	Character: if \code{method=pmax}, at each position the state having the highest probability is chosen. If \code{method=prob}, at each position the state is generated using the corresponding probability distribution taken from \code{object}.
}
  \item{L}{
	Integer: Maximal depth used to extract the probability distributions from the PST object.  
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(s1)
s1.seq <- seqdef(s1)
S1 <- pstree(s1, L=3)
generate(S1, n=10, l=10, method="prob")
## First state is generated with p(a)=0.9 and p(b)=0.1
generate(S1, n=10, l=10, method="prob", p1=c(0.9, 0.1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line