function [mask] = grabcut_v1(img, rect, others)
    height = size(img, 1);
    width = size(img, 2);
    topband = rect(1);
    bottomband = rect(2);
    leftband = rect(3);
    rightband = rect(4);
    outputDetail = others(1);
    reverseFG = others(2);
    insideoutTag = reverseFG;
    if (topband==0 && bottomband==0 && leftband==0 && rightband==0 && insideoutTag==0)  %no background priors means full foreground, direct set to full-foreground and return
        mask = true(height,width);                                                   
        if outputDetail==1
            figure,imshow(uint8(mask)*255); title('No background; Full foreground');
        end
        return;
    end
    yStart=max([1, topband+1]);
    yEnd=min([height, height-bottomband]);
    xStart=max([1, leftband+1]);
    xEnd=min([width, width-rightband]);
    mask = false(height, width);
    mask(yStart:yEnd, xStart:xEnd) = true;
    if reverseFG
        mask = ~mask;
    end
    modelSize = 13;
    componentsCount = 5;
    gmm = struct('model', {zeros(1, modelSize*componentsCount)}, 'coefs', {1}, 'meanidx', {6}, 'covidx', {21}, 'totalSampleCount', {0}, 'sums', {zeros(componentsCount, 3)}, 'prods', {zeros(componentsCount, 3, 3)}, 'sampleCounts', {zeros(1, componentsCount)}, 'inverseCovs', {zeros(componentsCount, 3, 3)}, 'covDeterms', {zeros(1, componentsCount)});
    bgmm = gmm;
    fgmm = gmm;
    gamma = 50;
    lambda = gamma * 9;
    leftW = zeros(height, width);
    upleftW = zeros(height, width);
    upW = zeros(height, width);
    uprightW = zeros(height, width);
    compidx = zeros(height, width);
    %ts = clock;
    beta = calcBeta(img);
    %t0 = clock;
    %fprintf('beta time %d\n', etime(t0, ts));
    [bgmm, fgmm] = initGMMs(img, mask, bgmm, fgmm);
    %t1 = clock;
    %fprintf('init time %d\n', etime(t1, t0));
    [leftW, upleftW, upW, uprightW] = calcNWeights(img, leftW, upleftW, upW, uprightW, beta, gamma);
    %t2 = clock;
    %fprintf('calc time %d\n', etime(t2, t1));
    iterCount = 1;
    for i=1:iterCount
        compidx = assignGMMsComponents(img, mask, bgmm, fgmm, compidx);
        %t3 = clock;
        %fprintf('assign time %d\n', etime(t3, t2));
        [bgmm, fgmm] = learnGMMs(img, mask, compidx, bgmm, fgmm);
        %t4 = clock;
        %fprintf('learn time %d\n', etime(t4, t3));
        [G, s, t] = constructGraph(img, mask, bgmm, fgmm, lambda, leftW, upleftW, upW, uprightW);
        %t5 = clock;
        %fprintf('const time %d\n', etime(t5, t4));
        [mask] = runGraph(G, s, t, mask);
        %t6 = clock;
        %fprintf('run time %d\n', etime(t6, t5));
        if outputDetail
            figure, imshow(img .* uint8(mask));
        end
    end
end
function [res] = get_color(gmm, color)
    res = 0;
    res = res + gmm.model(1) * get_color_ci(gmm, 1, color);
    res = res + gmm.model(2) * get_color_ci(gmm, 2, color);
    res = res + gmm.model(3) * get_color_ci(gmm, 3, color);
    res = res + gmm.model(4) * get_color_ci(gmm, 4, color);
    res = res + gmm.model(5) * get_color_ci(gmm, 5, color);
    epsilon = 0.00001;
    if res >= 1 - epsilon
        res = 1 - epsilon;
    end
end
function [res] = get_color_ci(gmm, ci, color)
    res = 0;
    if (gmm.model(ci) > 0)
        diff = double(color);
        diff = diff - gmm.model(3+3*ci:5+3*ci);
        mult = diff(1) * (diff(1)*gmm.inverseCovs(ci, 1, 1) + diff(2)*gmm.inverseCovs(ci, 2, 1) + diff(3)*gmm.inverseCovs(ci, 3, 1));
        mult = mult + diff(2) * (diff(1)*gmm.inverseCovs(ci, 1, 2) + diff(2)*gmm.inverseCovs(ci, 2, 2) + diff(3)*gmm.inverseCovs(ci, 3, 2));
        mult = mult + diff(3) * (diff(1)*gmm.inverseCovs(ci, 1, 3) + diff(2)*gmm.inverseCovs(ci, 2, 3) + diff(3)*gmm.inverseCovs(ci, 3, 3));
        res = 1 / sqrt(gmm.covDeterms(ci)) * exp(-0.5 * mult);
    end
end
function [k] = which_component(gmm, color)
    k = 0;
    maxv = 0;
    for ci=1:5
        p = get_color_ci(gmm, ci, color);
        if (p > maxv)
            k = ci;
            maxv = p;
        end
    end
end
function [gmm] = addSample(gmm, ci, color)
    % color 1*3
    gmm.sums(ci, :) = gmm.sums(ci, :) + color;
    gmm.prods(ci, 1, 1) = gmm.prods(ci, 1, 1) + color(1) * color(1);
    gmm.prods(ci, 1, 2) = gmm.prods(ci, 1, 2) + color(1) * color(2);
    gmm.prods(ci, 1, 3) = gmm.prods(ci, 1, 3) + color(1) * color(3);
    gmm.prods(ci, 2, 1) = gmm.prods(ci, 2, 1) + color(2) * color(1);
    gmm.prods(ci, 2, 2) = gmm.prods(ci, 2, 2) + color(2) * color(2);
    gmm.prods(ci, 2, 3) = gmm.prods(ci, 2, 3) + color(2) * color(3);
    gmm.prods(ci, 3, 1) = gmm.prods(ci, 3, 1) + color(3) * color(1);
    gmm.prods(ci, 3, 2) = gmm.prods(ci, 3, 2) + color(3) * color(2);
    gmm.prods(ci, 3, 3) = gmm.prods(ci, 3, 3) + color(3) * color(3);
    gmm.sampleCounts(ci) = gmm.sampleCounts(ci) + 1;
    gmm.totalSampleCount = gmm.totalSampleCount + 1;
end
function [gmm] = init_learn(gmm)
    gmm.sums = zeros(5, 3);
    gmm.prods = zeros(5, 3, 3);
    gmm.sampleCounts = zeros(1, 5);
    gmm.totalSampleCount = 0;
end
function [gmm] = end_learn(gmm)
    variance = 0.01;
    for ci=1:5
        n = gmm.sampleCounts(ci);
        if (n==0)
            gmm.model(ci) = 0;
        else
            gmm.model(ci) = n / gmm.totalSampleCount;
            gmm.model(3+3*ci) = gmm.sums(ci, 1) / n;
            gmm.model(4+3*ci) = gmm.sums(ci, 2) / n;
            gmm.model(5+3*ci) = gmm.sums(ci, 3) / n;
            gmm.model(12+9*ci) = gmm.prods(ci, 1, 1) / n - gmm.model(3+3*ci) * gmm.model(3+3*ci);
            gmm.model(13+9*ci) = gmm.prods(ci, 1, 2) / n - gmm.model(3+3*ci) * gmm.model(4+3*ci);
            gmm.model(14+9*ci) = gmm.prods(ci, 1, 3) / n - gmm.model(3+3*ci) * gmm.model(5+3*ci);
            gmm.model(15+9*ci) = gmm.prods(ci, 2, 1) / n - gmm.model(4+3*ci) * gmm.model(3+3*ci);
            gmm.model(16+9*ci) = gmm.prods(ci, 2, 2) / n - gmm.model(4+3*ci) * gmm.model(4+3*ci);
            gmm.model(17+9*ci) = gmm.prods(ci, 2, 3) / n - gmm.model(4+3*ci) * gmm.model(5+3*ci);
            gmm.model(18+9*ci) = gmm.prods(ci, 3, 1) / n - gmm.model(5+3*ci) * gmm.model(3+3*ci);
            gmm.model(19+9*ci) = gmm.prods(ci, 3, 2) / n - gmm.model(5+3*ci) * gmm.model(4+3*ci);
            gmm.model(20+9*ci) = gmm.prods(ci, 3, 3) / n - gmm.model(5+3*ci) * gmm.model(5+3*ci);
            dtrm = gmm.model(12+9*ci) * (gmm.model(16+9*ci)*gmm.model(20+9*ci)-gmm.model(17+9*ci)*gmm.model(19+9*ci)) - gmm.model(13+9*ci) * (gmm.model(15+9*ci)*gmm.model(20+9*ci)-gmm.model(17+9*ci)*gmm.model(18+9*ci)) + gmm.model(14+9*ci) * (gmm.model(15+9*ci)*gmm.model(19+9*ci)-gmm.model(16+9*ci)*gmm.model(18+9*ci));
            epsilon = 0.00001;
            if (dtrm <= epsilon)
                gmm.model(12+9*ci) = gmm.model(12+9*ci) + variance;
                gmm.model(16+9*ci) = gmm.model(16+9*ci) + variance;
                gmm.model(20+9*ci) = gmm.model(20+9*ci) + variance;
            end
            gmm = calcInverseCovAndDeterm(gmm, ci);
        end
    end
end
function [gmm] = calcInverseCovAndDeterm(gmm, ci)
    if (gmm.model(ci) > 0)
        gmm.covDeterms(ci) = gmm.model(12+9*ci) * (gmm.model(16+9*ci)*gmm.model(20+9*ci)-gmm.model(17+9*ci)*gmm.model(19+9*ci)) - gmm.model(13+9*ci) * (gmm.model(15+9*ci)*gmm.model(20+9*ci)-gmm.model(17+9*ci)*gmm.model(18+9*ci)) + gmm.model(14+9*ci) * (gmm.model(15+9*ci)*gmm.model(19+9*ci)-gmm.model(16+9*ci)*gmm.model(18+9*ci));
        dtrm = gmm.covDeterms(ci);
        gmm.inverseCovs(ci, 1, 1) = (gmm.model(16+9*ci)*gmm.model(20+9*ci) - gmm.model(17+9*ci)*gmm.model(19+9*ci)) / dtrm;
        gmm.inverseCovs(ci, 1, 2) = -(gmm.model(15+9*ci)*gmm.model(20+9*ci) - gmm.model(17+9*ci)*gmm.model(18+9*ci)) / dtrm;
        gmm.inverseCovs(ci, 1, 3) = (gmm.model(15+9*ci)*gmm.model(19+9*ci) - gmm.model(16+9*ci)*gmm.model(18+9*ci)) / dtrm;
        gmm.inverseCovs(ci, 2, 1) = -(gmm.model(13+9*ci)*gmm.model(20+9*ci) - gmm.model(14+9*ci)*gmm.model(19+9*ci)) / dtrm;
        gmm.inverseCovs(ci, 2, 2) = (gmm.model(12+9*ci)*gmm.model(20+9*ci) - gmm.model(14+9*ci)*gmm.model(18+9*ci)) / dtrm;
        gmm.inverseCovs(ci, 2, 3) = -(gmm.model(12+9*ci)*gmm.model(19+9*ci) - gmm.model(13+9*ci)*gmm.model(18+9*ci)) / dtrm;
        gmm.inverseCovs(ci, 3, 1) = (gmm.model(13+9*ci)*gmm.model(17+9*ci) - gmm.model(14+9*ci)*gmm.model(16+9*ci)) / dtrm;
        gmm.inverseCovs(ci, 3, 2) = -(gmm.model(12+9*ci)*gmm.model(17+9*ci) - gmm.model(14+9*ci)*gmm.model(15+9*ci)) / dtrm;
        gmm.inverseCovs(ci, 3, 3) = (gmm.model(12+9*ci)*gmm.model(16+9*ci) - gmm.model(13+9*ci)*gmm.model(15+9*ci)) / dtrm;
    end
end
function [beta] = calcBeta(img)
    beta = 0;
    height = size(img, 1);
    width = size(img, 2);
    img = double(img);
    % y>1
    img1 = img(2:height, :, :);
    img2 = img(1:height-1, :, :);
    img3 = img2 - img1;
    beta = beta + sum(sum(dot(img3, img3, 3)));
    % y>1 x>1
    img1 = img(2:height, 2:width, :);
    img2 = img(1:height-1, 1:width-1, :);
    img3 = img2 - img1;
    beta = beta + sum(sum(dot(img3, img3, 3)));
    % x>1
    img1 = img(:, 2:width, :);
    img2 = img(:, 1:width-1, :);
    img3 = img2 - img1;
    beta = beta + sum(sum(dot(img3, img3, 3)));
    % y>1 x<width
    img1 = img(2:height, 1:width-1, :);
    img2 = img(1:height-1, 2:width, :);
    img3 = img2 - img1;
    beta = beta + sum(sum(dot(img3, img3, 3)));

    epsilon = 0.00001;
    if beta <= epsilon
        beta = 0;
    else
        beta = 1 / (2 * beta / (4*height*width - 3*height - 3*width + 2));
    end
end
function [leftW, upleftW, upW, uprightW] = calcNWeights(img, leftW, upleftW, upW, uprightW, beta, gamma)
    gammaDiv = gamma / sqrt(2.0);
    height = size(img, 1);
    width = size(img, 2);
    img = double(img);
    % y>1
    img1 = img(2:height, :, :);
    img2 = img(1:height-1, :, :);
    img3 = img2 - img1;
    upW(2:height, :) = gamma * exp(-beta * dot(img3, img3, 3));
    % y>1 x>1
    img1 = img(2:height, 2:width, :);
    img2 = img(1:height-1, 1:width-1, :);
    img3 = img2 - img1;
    upleftW(2:height, 2:width) = gammaDiv * exp(-beta * dot(img3, img3, 3));
    % x>1
    img1 = img(:, 2:width, :);
    img2 = img(:, 1:width-1, :);
    img3 = img2 - img1;
    leftW(:, 2:width) = gamma * exp(-beta * dot(img3, img3, 3));
    % y>1 x<width
    img1 = img(2:height, 1:width-1, :);
    img2 = img(1:height-1, 2:width, :);
    img3 = img2 - img1;
    uprightW(2:height, 1:width-1) = gammaDiv * exp(-beta * dot(img3, img3, 3));
end
function [bgmm, fgmm] = initGMMs(img, mask, bgmm, fgmm)
    kMeansCount = 100;
    height = size(img, 1);
    width = size(img, 2);
    img = double(img);
    fgSize = sum(sum(mask==1));
    bgSize = height*width - fgSize;
    fgSamples = zeros(fgSize, 3);
    bgSamples = zeros(bgSize, 3);
    img1 = img(:, :, 1);
    img2 = img(:, :, 2);
    img3 = img(:, :, 3);
    inds = find(mask==true);
    fgSamples(:, 1) = img1(inds);
    fgSamples(:, 2) = img2(inds);
    fgSamples(:, 3) = img3(inds);
    inds = find(mask==false);
    bgSamples(:, 1) = img1(inds);
    bgSamples(:, 2) = img2(inds);
    bgSamples(:, 3) = img3(inds);

    bgidx = kmeans(bgSamples, 5, 'MaxIter', kMeansCount);
    bgmm = init_learn(bgmm);
    for i=1:bgSize
        bgmm = addSample(bgmm, bgidx(i), bgSamples(i, :));
    end
    bgmm = end_learn(bgmm);
    
    fgidx = kmeans(fgSamples, 5, 'MaxIter', kMeansCount);
    fgmm = init_learn(fgmm);
    for i=1:fgSize
        fgmm = addSample(fgmm, fgidx(i), fgSamples(i, :));
    end
    fgmm = end_learn(fgmm);
end
function [compidx] = assignGMMsComponents(img, mask, bgmm, fgmm, compidx)
    height = size(img, 1);
    width = size(img, 2);
    for x=1:width
        for y=1:height
            color = double(squeeze(img(y, x, :)));
            if (mask(y, x)==false)
                compidx(y, x) = which_component(bgmm, color);
            else
                compidx(y, x) = which_component(fgmm, color);
            end
        end
    end
end
function [bgmm, fgmm] = learnGMMs(img, mask, compidx, bgmm, fgmm)
    bgmm = init_learn(bgmm);
    fgmm = init_learn(fgmm);
    height = size(img, 1);
    width = size(img, 2);
    for x=1:width
        for y=1:height
            for ci=1:5
                if (compidx(y, x)==ci)
                    color = double(squeeze(img(y, x, :))');
                    if (mask(y, x) == false)
                        bgmm = addSample(bgmm, ci, color);
                    else
                        fgmm = addSample(fgmm, ci, color);
                    end
                    break;
                end
            end
        end
    end
    bgmm = end_learn(bgmm);
    fgmm = end_learn(fgmm);
end
function [G, start_point, end_point] = constructGraph(img, mask, bgmm, fgmm, lambda, leftW, upleftW, upW, uprightW)
    height = size(img, 1);
    width = size(img, 2);
    nodeNum = height*width+2;
    edgeNum = (4*height*width - 3*(height+width) + 2)*2 + 2*(height*width);
    start_point = nodeNum-1;
    end_point = nodeNum;
    s = zeros(1, edgeNum);
    t = zeros(1, edgeNum);
    w = zeros(1, edgeNum);
    imgsize = height*width;
    s(1:imgsize) = start_point;
    t(1:imgsize) = 1:imgsize;
    s(imgsize+1:imgsize+imgsize) = 1:imgsize;
    t(imgsize+1:imgsize+imgsize) = end_point;
    w(1:imgsize) = 0;
    w(imgsize+1:imgsize+imgsize) = lambda;
    iterP = 1;
    for x=1:width
        for y=1:height
            color = double(squeeze(img(y, x, :))');
            if (mask(y, x) == true) % bg
                fromSource = -log(get_color(bgmm, color));
                toSink = -log(get_color(fgmm, color));
                w(iterP) = fromSource;
                w(iterP + height*width) = toSink;
            end
            iterP = iterP + 1;
        end
    end
        iterP = 2*height*width + 1;
    curNode = 1;
    for x=1:width
        for y=1:height
            if (x>1)
                s(iterP) = curNode;
                t(iterP) = curNode-height;
                w(iterP) = leftW(y, x);
                s(iterP+1) = t(iterP);
                t(iterP+1) = curNode;
                w(iterP+1) = w(iterP);
                iterP = iterP + 2;
            end
            if (x>1 && y>1)
                s(iterP) = curNode;
                t(iterP) = curNode-1-height;
                w(iterP) = upleftW(y, x);
                s(iterP+1) = t(iterP);
                t(iterP+1) = curNode;
                w(iterP+1) = w(iterP);
                iterP = iterP + 2;
            end
            if (y>1)
                s(iterP) = curNode;
                t(iterP) = curNode-1;
                w(iterP) = upW(y, x);
                s(iterP+1) = t(iterP);
                t(iterP+1) = curNode;
                w(iterP+1) = w(iterP);
                iterP = iterP + 2;
            end
            if (x<width && y>1)
                s(iterP) = curNode;
                t(iterP) = curNode-1+height;
                w(iterP) = uprightW(y, x);
                s(iterP+1) = t(iterP);
                t(iterP+1) = curNode;
                w(iterP+1) = w(iterP);
                iterP = iterP + 2;
            end
            curNode = curNode + 1;
        end
    end
    %imgpos = zeros(height, width);
    %imgpos(:) = 1:height*width;
    %% x>1
    %iterP = 2*height*width + 1;
    %iterE = iterP + height*(width-1)-1;
    %s(iterP:iterE) = height+1:width*height;
    %t(iterP:iterE) = 1:(width-1)*height;
    %w(iterP:iterE) = leftW(height+1:width*height);
    %s(iterP+height*(width-1):iterE+height*(width-1)) = t(iterP:iterE);
    %t(iterP+height*(width-1):iterE+height*(width-1)) = s(iterP:iterE);
    %w(iterP+height*(width-1):iterE+height*(width-1)) = w(iterP:iterE);
    %% y>1
    %iterP = iterP + 2*(width-1)*height;
    %iterE = iterP - 1 + width*(height-1);
    %pos1 = imgpos(2:height, :);
    %pos2 = imgpos(1:height-1, :);
    %pos1 = pos1(:);
    %pos2 = pos2(:);
    %s(iterP:iterE) = pos1;
    %t(iterP:iterE) = pos2;
    %w(iterP:iterE) = upW(pos1);
    %s(iterP+width*(height-1):iterE+width*(height-1)) = t(iterP:iterE);
    %t(iterP+width*(height-1):iterE+width*(height-1)) = s(iterP:iterE);
    %w(iterP+width*(height-1):iterE+width*(height-1)) = w(iterP:iterE);
    %% x>1 y>1
    %iterP = iterP + 2*(height-1)*width;
    %iterE = iterP - 1 + (width-1)*(height-1);
    %pos1 = imgpos(2:height, 2:width);
    %pos2 = imgpos(1:height-1, 1:width-1);
    %pos1 = pos1(:);
    %pos2 = pos2(:);
    %s(iterP:iterE) = pos1;
    %t(iterP:iterE) = pos2;
    %w(iterP:iterE) = upleftW(pos1);
    %s(iterP+(width-1)*(height-1):iterE+(width-1)*(height-1)) = t(iterP:iterE);
    %t(iterP+(width-1)*(height-1):iterE+(width-1)*(height-1)) = s(iterP:iterE);
    %w(iterP+(width-1)*(height-1):iterE+(width-1)*(height-1)) = w(iterP:iterE);
    %% y>1 x<width
    %iterP = iterP + 2*(height-1)*(width-1);
    %iterE = iterP - 1 + (width-1)*(height-1);
    %pos1 = imgpos(2:height, 1:width-1);
    %pos2 = imgpos(1:height-1, 2:width);
    %pos1 = pos1(:);
    %pos2 = pos2(:);
    %s(iterP:iterE) = pos1;
    %t(iterP:iterE) = pos2;
    %w(iterP:iterE) = uprightW(pos1);
    %s(iterP+(width-1)*(height-1):iterE+(width-1)*(height-1)) = t(iterP:iterE);
    %t(iterP+(width-1)*(height-1):iterE+(width-1)*(height-1)) = s(iterP:iterE);
    %w(iterP+(width-1)*(height-1):iterE+(width-1)*(height-1)) = w(iterP:iterE);
    G = digraph(s, t, w);
end
function [mask, cs, ct] = runGraph(G, s, t, mask)
    height = size(mask, 1);
    width = size(mask, 2);
    [~, ~, cs, ct] = maxflow(G, s, t);
    mask(cs(cs<=height*width)) = true;
    mask(ct(ct<=height*width)) = false;
end
