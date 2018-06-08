close all;
iptsetpref('ImshowBorder','loose');

[filename,pathname]=uigetfile('*.*','Select image file');
longfilename = strcat(pathname,filename);
photo = imread(longfilename);
[imageHeight,imageWidth,imageDepth]=size(photo);

figure,imshow(photo,'InitialMagnification',100);
title('Drag a ROI rectangle by mouse to start segmentation');

rect = round(getrect());       %rect is a four-element vector with the form [xmin ymin width height], xmin and ymin may be negative.
x_left=max(rect(1),0);
x_right =min(rect(1)+rect(3),imageWidth-1);
y_top =max(rect(2),0);
y_bottom=min(rect(2)+rect(4),imageHeight-1);
corners=[x_left,y_top,x_right,y_bottom];

hold on;
plot([x_left+1,  x_right+1],[y_top+1,   y_top+1   ],'LineWidth', 1, 'Color', 'b');
plot([x_left+1,  x_right+1],[y_bottom+1,y_bottom+1],'LineWidth', 1, 'Color', 'b');
plot([x_left+1,  x_left+1 ],[y_top+1,   y_bottom+1],'LineWidth', 1, 'Color', 'b');
plot([x_right+1, x_right+1],[y_top+1,   y_bottom+1],'LineWidth', 1, 'Color', 'b');
drawnow;

fprintf('Processing grabcut...\n');
parameters(1)=y_top;
parameters(2)=imageHeight-1-y_bottom;
parameters(3)=x_left;
parameters(4)=imageWidth-1-x_right;
parameters;
others = [1, 0];
% other(1): detail
% other(2): 1 fg; 0 bg
tic;
[mask] = grabcut(photo, parameters, others);
time = toc;
disp(time);
%imwrite(photo .* uint8(mask), 'result.jpg');
fprintf('finish\n');
