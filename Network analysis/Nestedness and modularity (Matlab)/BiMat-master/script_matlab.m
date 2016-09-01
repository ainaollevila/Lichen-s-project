A = importdata('mymatrix.txt')
bp = Bipartite(A);
row_labels = {'1', '2', '3', '4', '5', '6', '7', '8', '9', '10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32','33','34','35'};
cols_labels = {'1', '2', '3', '4', '5', '6', '7', '8', '9', '10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32','33','34','35','36','37','38','39','40','41','42','43','44','45','46','47','48','49','50','51'};
row_ids=[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35];
col_ids=[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51];
bp.row_labels = row_labels;
bp.col_labels = col_labels;
bp.row_class = row_ids;
bp.col_class = col_ids;
bp.printer.PrintGeneralProperties();
bp.community = LeadingEigenvector(bp.matrix); 
bp.community.DoKernighanLinTunning = true;
bp.community.Detect();
fprintf('The modularity value Qb is %f\n', bp.community.Qb);
fprintf('The fraction inside modules Qr is %f\n',bp.community.Qr);
bp.nestedness.Detect();
fprintf('The Nestedness value is %f\n', bp.nestedness.N);
bp.printer.PrintStructureValues();


%%
figure(1);
set(gcf,'Position',[0 72 1751 922]);
bp.plotter.use_type_interaction = true;
bp.plotter.color_interactions(1,:) = [1 0 0];
bp.plotter.color_interactions(2,:) = [0 0 1];
bp.plotter.back_color = 'white';
bp.plotter.PlotMatrix();

%%
figure(2);
set(gcf,'Position',[0+50 72 932 922]);
bp.plotter.use_isocline = true;
bp.plotter.isocline_color = 'red';
bp.plotter.PlotNestedMatrix();

%%
figure(3);
set(gcf,'Position',[0+100 72 1754 922]);
subplot(1,2,1);
bp.community = LPBrim(bp.matrix);
bp.plotter.use_isocline = true;
bp.plotter.PlotModularMatrix();
%%
title(['$Q = $',num2str(bp.community.Qb),' $c = $', num2str(bp.community.N)],'interpreter','latex','fontsize',23);
%%
subplot(1,2,2);
bp.community = LPBrim(bp.matrix);
bp.community.optimize_by_component = true;
bp.plotter.PlotModularMatrix();
%%
title(['$Q = $',num2str(bp.community.Qb),' $c = $', num2str(bp.community.N)],'interpreter','latex','fontsize',23);
%%
set(gca,'position',get(gca,'position')-[0.07 0 0 0]);

%%
figure(4);
set(gcf,'Position',[0+150 72 1754 922]);
bp.community = LeadingEigenvector(bp.matrix);
%%
subplot(1,2,1);
bp.plotter.use_isocline = false;
bp.plotter.use_type_interaction = false;
bp.plotter.PlotModularMatrix();
%%
subplot(1,2,2);
bp.plotter.use_module_format = false;
bp.plotter.use_isocline = true;
bp.plotter.isocline_color = 'red';
bp.plotter.division_color = 'red';
bp.plotter.back_color = [0 100 180]/255;
bp.plotter.cell_color = 'white';
bp.plotter.PlotModularMatrix();
set(gca,'position',get(gca,'position')-[0.07 0 0 0]);

%%
figure(6);
set(gcf,'Position',[19+800 72 932 922]);
bp.plotter.PlotModularGraph();

%Statistics
bp.statistics.DoCompleteAnalysis(100, @NullModels.EQUIPROBABLE);
bp.statistics.DoCompleteAnalysis(100, @NullModels.AVERAGE);
bp.statistics.DoCompleteAnalysis(100, @NullModels.AVERAGE_COLS);
bp.statistics.DoCompleteAnalysis(100, @NullModels.AVERAGE_ROWS);
bp.statistics.DoCompleteAnalysis(100, @NullModels.FIXED);
bp.printer.PrintStructureStatistics();
bp.internal_statistics.TestInternalModules(100,@NullModels.EQUIPROBABLE);
bp.internal_statistics.TestDiversityRows(1000);
