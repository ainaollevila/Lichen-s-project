%% Getting Started
%
%% Download
%
% * <https://github.com/cesar7f/BiMat/archive/master.zip BiMat Ver. 1.0> - *April 2015*
%
%% Installation
% To install |BiMat|, copy the downloaded zip file to a directory of interest and unzip it. Next, you
% will need to add |BiMat| to the |Matlab| path either temporally or permanently:
%
% *Temporal path:* Add the |BiMat| directory (and sub-directories) to the
% \matlab path. The user will need to type the next lines everytime he starts a new |Matlab| session:
%Replace next line with your appropiate path'
bimat_user_path = 'mypackages/bimat';
g=genpath(bimat_user_path);
addpath(g);	 
%%
% *Permanent path:* Alternatively, the user can add |BiMat| to the |Matlab| search path permanently. 
% Instructions about how to do that can be found on: 
% <http://www.mathworks.com/help/matlab/matlab_env/add-remove-or-reorder-folders-on-the-search-path.html Matlab Search Path>
%
%% Configuration
%
% Before start using |BiMat| the user can check how to configure the
% default values by consulting ...
%
%% Start Guide
%
% You can download the start guide in the link below. This document contains
% complete examples to start using |BiMat|, which complements the examples
% found in this website:
%
% * <https://github.com/cesar7f/BiMat/archive/master.zip Start Guide>
%
%% |Matlab| extensive documentation
%
% |BiMat| comes extensively documented.
% For accessing this documentation, the user can use directly inside |Matlab|
% command prompt either |help| or |doc| functions following by the name of
% the class of method the user is interested as in the next examples:
doc Bipartite; %access class documentation
doc BipartiteModularity.bb %accesing a class property documentation
doc StatisticalTest.DoCompleteAnalysis; %assesing a method documentation
%and so on...