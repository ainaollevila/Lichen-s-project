g=genpath('/Users/aina/Dropbox/Lichen-s-project/Network analysis/Nestedness and modularity (Matlab)/BiMat-master');
addpath(g);
%%
i=1
string = sprintf('/Users/aina/Dropbox/Lichen-s-project/dev/data/cooccurence_matrix%d.csv',i)
string = sprintf('/Users/aina/Dropbox/Lichen-s-project/dev/data/matrix%d.txt',i)
%A = csvread(string)
A = importdata(string)
%%

path = '/Users/aina/Dropbox/Lichen-s-project/dev/data/Simon'
%%
myFolderInfo = dir ('/Users/aina/Dropbox/Lichen-s-project/dev/data/Simon/')



myFolderInfo(3).name
%%


stringlist = sprintf('%s/fileslist.txt',path)
diary(stringlist)
diary on
for i=4:196
    i
    myFolderInfo(i).name
end
diary off


%%

for i=4:196
string = sprintf('%s/%s',path,myFolderInfo(i).name)
%string = sprintf('%s/m%d.txt',path,i)

A = importdata(string)

%A = csvread(string,1,1)
% size(A) 
% rows = size(A,1)
% cols = size(A,2)
% for j = 1:rows
%     for k=1:cols
%         if A(j,k) > 0
%             A(j,k) = 1
%         end
%     end
% end
bp = Bipartite(A);
string2 = sprintf('%s/mat%d_genprop.txt',path,i)
diary(string2)
bp.printer.PrintGeneralProperties();

%Calculating modularity and nestedness
diary off
bp.community = LeadingEigenvector(bp.matrix); 
bp.community.DoKernighanLinTunning = true;
bp.community.Detect();
bp.nestedness.Detect();
diary on
bp.printer.PrintStructureValues();
diary off

string_fig1 = sprintf('%s/mat%d_fig1.pdf',path,i)
figure(1);
%set(gcf,'Position',[0 72 1751 922]);
bp.plotter.use_type_interaction = true;
bp.plotter.color_interactions(1,:) = [1 0 0];
bp.plotter.color_interactions(2,:) = [0 0 1];
bp.plotter.back_color = 'white';
bp.plotter.PlotMatrix();
print(string_fig1,'-dpdf')
close

figure(2);
string_fig2 = sprintf('%s/mat%d_fig2.pdf',path,i)
%set(gcf,'Position',[0+50 72 932 922]);
bp.plotter.use_isocline = true;
bp.plotter.isocline_color = 'red';
bp.plotter.PlotNestedMatrix();
print(string_fig2,'-dpdf')
close

figure(3);
string_fig3 = sprintf('%s/mat%d_fig3.pdf',path,i)
%set(gcf,'Position',[0+100 72 1754 922]);
subplot(1,2,1);
bp.community = LPBrim(bp.matrix);
bp.plotter.use_isocline = true;
bp.plotter.PlotModularMatrix();
title(['$Q = $',num2str(bp.community.Qb),' $c = $', num2str(bp.community.N)],'interpreter','latex');
subplot(1,2,2);
bp.community = LPBrim(bp.matrix);
bp.community.optimize_by_component = true;
bp.plotter.PlotModularMatrix();
title(['$Q = $',num2str(bp.community.Qb),' $c = $', num2str(bp.community.N)],'interpreter','latex');
print(string_fig3,'-dpdf')
close

figure(4);
string_fig4 = sprintf('%s/mat%d_fig4.pdf',path,i)

%set(gcf,'Position',[0+150 72 1754 922]);
bp.community = LeadingEigenvector(bp.matrix);

subplot(1,2,1);
bp.plotter.use_isocline = false;
bp.plotter.use_type_interaction = false;
bp.plotter.PlotModularMatrix();

subplot(1,2,2);
bp.plotter.use_module_format = false;
bp.plotter.use_isocline = true;
bp.plotter.isocline_color = 'red';
bp.plotter.division_color = 'red';
bp.plotter.back_color = [0 100 180]/255;
bp.plotter.cell_color = 'white';
bp.plotter.PlotModularMatrix();
%set(gca,'position',get(gca,'position')-[0.07 0 0 0]);
print(string_fig4,'-dpdf')
close

figure(6);
%set(gcf,'Position',[19+800 72 932 922]);
string_fig6 = sprintf('%s/mat%d_fig6.pdf',path,i)
bp.plotter.PlotModularGraph();
print(string_fig6,'-dpdf')
close

%Statistics
string_statistics = sprintf('%s/mat%d_Statistics.txt',path,i)
diary(string_statistics)
diary off
bp.statistics.DoCompleteAnalysis(100, @NullModels.EQUIPROBABLE);
diary on
bp.printer.PrintStructureStatistics();
end 
%%
diary off
bp.statistics.DoCompleteAnalysis(100, @NullModels.AVERAGE);
diary on
bp.printer.PrintStructureStatistics();
diary off

bp.statistics.DoCompleteAnalysis(100, @NullModels.AVERAGE_COLS);
diary on
bp.printer.PrintStructureStatistics();
diary off

bp.statistics.DoCompleteAnalysis(100, @NullModels.AVERAGE_ROWS);
diary on
bp.printer.PrintStructureStatistics();
diary off

bp.statistics.DoCompleteAnalysis(100, @NullModels.FIXED);
diary on
bp.printer.PrintStructureStatistics();
diary off

%end
