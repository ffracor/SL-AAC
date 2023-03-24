data = readtable('dataset.csv'); 

x = (data.height_Cm);
y = (data.weight_Kg);
z = (data.position);

figure
for i=1:size(x,1)
    hold on
    role = z{i};    
    switch(role)
        case 'Midfielder'
            color = 'r';
        case 'Forward'
            color = 'g';
        case 'Defender'
            color = 'b';
        otherwise
            color = 'c';
    end
    scatter(x(i),y(i),[],color,'filled')
end