\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{courseclass}[2003/06/15 v3.0 (dnr)]
\LoadClass[11pt]{article}
\RequirePackage[margin=72pt]{geometry} % Remove this line to go back to normal LaTeX margins - agayer 2009.01.16

% set up the page styles 
% (one for first page, one for subsequent pages)

\newcommand{\ps@firsthead}{ %
  \renewcommand{\@oddhead}{ %
    \textbf{\CSclassnum\hfill} %
    \CSclassname\hfill %
    \makebox[\CSclassnumwidth][r]{\CSprof} %
  } %
  \renewcommand{\@oddfoot}{} %
  \renewcommand{\@evenhead}{\@oddhead} %
  \renewcommand{\@evenfoot}{\@oddfoot} %
}
\newcommand{\ps@head}{ %
  \renewcommand{\@oddhead}{ %
    {\CSclassnum}
    \hfill %
    \asgnname\hfill %
    {\makebox[\CSclassnumwidth][r]{\textbf{\CSduedate}}} %
}
  \renewcommand{\@oddfoot}{\hfill \thepage \hfill} %
  \renewcommand{\@evenhead}{\@oddhead} %
  \renewcommand{\@evenfoot}{\@oddfoot} %
}

% vim: ft=tex

