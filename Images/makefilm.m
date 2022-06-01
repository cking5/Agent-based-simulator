%%%%%%%%%%%%%%%
%% make illustrative video from simulation output


files = dir(fullfile('*.jpg'));
imageNames = {files.name};

%outputVideo = VideoWriter('test.avi','Grayscale AVI');
outputVideo = VideoWriter('asym_horseshoe_(0,0,0).mp4','MPEG-4');
%outputVideo.Quality = 50;
%outputVideo.FrameRate = 50;
open(outputVideo)
cc = 0;
for ii = 1:length(imageNames)

   img1 = imread(imageNames{ii});
   img = img1;
   %img = rgb2gray(img1);
   writeVideo(outputVideo,img)

end

close(outputVideo)
