function revlRun(sID, img_pair, is_food_rewarded_first, subject_code, taskVersion, block)
% FUNCTION revlRun(sID, img_pair, is_food_rewarded_first, subject_code, taskVersion, block)
%
% sID: progressive id for each participant. You must use a unique
% identifier for each participant! For compatibility with previous data 
% collection, in the year 2019 the first subject will have sID = 501. To
% each subject you must assign the same sID!
%
% The argument img_pair defines the pair of images that are shown in each
% single block of trials. There are 8 blocks of trials and 8 image pairs.
% Before running a subject, in the Matlab console write the following
% instructions:
%
% img_pair = 1:8;
% img_pair(randperm(length(img_pair)))
%
% The last instruction generates a random sequence of 1:8 numbers that can
% be used, one for each block of trials, to determine the img_pair for a
% subject. For example: 
% 3     6     8     5     7     2     4     1
% requires that you assign the value of 3 to the argument img_pair the 
% first time you run the script for a subject; you assign the value of 6 to 
% the argument img_pair the second time you run the script for the same 
% subject, and so on.
%
% The argument is_food_rewarded_first take on the values 0 or 1. 
% For for each subjects, do the following:
%
% is_food_rewarded_first = [0 0 0 0 1 1 1 1];
% is_food_rewarded_first(randperm(length(is_food_rewarded_first)))
%
% For example, the output may be 0     0     0     1     1     1     0     1
% Thus, you will assign the value of 0 to the argument is_food_rewarded_first  
% the first time you run the script for a subject; you assign the value of 
% 0 to the argument is_food_rewarded_first the second time you run the script 
% for the same subject, and so on.
%
% The argument subject_code is a string that must be enclosed in double
% quotes. This string must be generate by means of the following rules.
% The subject_code string will include:
% the first two letters of the name;
% the first two letters of the surname;
% the date of birth, in the format: year/month/day: xxxx_xx_xx;
% the last 3 numbers of the cell phone;
% gender: f or m.
% For example, consider the subject Mario Rossi, born 1993_03_23, with
% telephone number 236 4397639. His code will be:
% "ma_ro_1993_03_23_639_m". Please use exactly this structure for each 
% participant!
% 
% The argument taskVersion determines whether the volatility is high (1) or
% low (2). In half of the blocks, the argument is equal to 1, in the
% other half is equal to 2. Again, do the following:
% 
% taskVersion = [1 1 1 1 2 2 2 2];
% taskVersion(randperm(length(taskVersion)))
% 
% For example, you get: 1     2     2     1     1     2     1     2
% So, the first time you run the script for a subject you assign the value 
% of 1 to the argument taskVersion; the second time you run the script 
% for the same subject taskVersion will be equal to 2, and so on.
%
% The argument block takes on the consecutive numbers 1, 2, ..., 8 in the
% eight consecutive blocks of the experiment.
%
% The present script will save the data to the 'data' subfolder. 
%
% TASK DESCRIPTION:
% 2 images are presented on every trial, each associated with either a 0.7 
% or a 0.3 probability of reward. 
% At various points during the task the identity of the high and low reward
% images will be reversed. Subjects have to continuously keep track of 
% which image is currently best. 
% In each block of low volatility trials there are 4 epochs, with 40 trials 
% each. In each block of high volatility trials there are 8 epochs, 
% with 20 trials each.
% The experiment parameters are set in the revlParams.m file. 
% 
% version 2019_04_18
%
% These are the instructions that can be used in the six runs of Mario 
% Rossi:
%
% revlRun(523, 6,  1, 'ma_ro_1993_03_23_639_m', 1, 1);
% revlRun(523, 4,  0, 'ma_ro_1993_03_23_639_m', 2, 2);
% revlRun(523, 1,  0, 'ma_ro_1993_03_23_639_m', 2, 3);
% revlRun(523, 2,  0, 'ma_ro_1993_03_23_639_m', 2, 4);
% revlRun(523, 3,  1, 'ma_ro_1993_03_23_639_m', 1, 5);
% revlRun(523, 5,  1, 'ma_ro_1993_03_23_639_m', 1, 6);
% revlRun(523, 7,  0, 'ma_ro_1993_03_23_639_m', 2, 7);
% revlRun(523, 8,  1, 'ma_ro_1993_03_23_639_m', 1, 8);
%

% -------------------------------------------------------------------------
%
% Explanation of the output!
%
% Volatility is high when taskVersion is 1;
% Volatility is low when taskVersion is 2;
%
% In the output file saved in the data folder, we have:  
% - data.prep.locs = nx2 matrix with the locations of the stimuli on the
% screen; the first column indicates the img on the left, the second column
% indicates the stimulus on the right. 
%
% Orange is the stimulus that is rewarded. In the first of the four epochs
% the orange image is rewarded in the first and third blocks; the blue
% image is rewarded (most) in the second and fourth blocks.
% When is_psychopathy_reward_first == 1, then the orange image corresponds
% to the violent images; blue corresponds to neutral.
% For the violent images, the probability of reward is 0.7, 0.3, 0.7, 0.3
% in the four epochs.
% When is_psychopathy_reward_first == 0, then the orange image corresponds
% to the neutral images; blue corresponds to violent.
% For the violent images, the probability of reward is 0.3, 0.7, 0.3, 0.7
% in the four epochs.
% In the output file, data.choice == 1 means "orange", data.choice == 0  
% means "blue".
% What is orange or blue depends on the value of the parameter
% is_psychopathy_reward_first.
% In the output file, data.outcome indicates whether a positive feedback 
% (1) or not (0) has been assigned to each participant's choice.
%==========================================================================
%
% The parameters of the reversal are set in the revlParams.m file, which
% include the block length, feedback probability and total experiment
% length. These can be changed. 
%
% Ignore what follows!
%
% If you open the output file in the data folder, Matlab asks if you want
% to 'generate' the data structure.  
% - data.choice = key pressed by subject
% - data.outcome = feedback
% - data.prep.locs = nx2 matrix with the locations of the stimuli on the
% screen; the first column indicates the img on the left, the second column
% indicates the stimulus on the right. In the revlParams.m, we have:
% prep.cue{1} = fullfile(prep.dir.imgs, 'slot1_up.jpg'); % blue
% prep.cue{2} = fullfile(prep.dir.imgs, 'slot2_up.jpg'); % orange
% So, "1" means the "blue" image, and "2" means the "orange" image. In our
% case, it is important to know which image corresponds to the choice of
% the subject, given that the two images will be "njre" and "cntr".
%==========================================================================

% check for Opengl compatibility, abort otherwise
AssertOpenGL;

% neither macbook nor testing computer seem to interact properly with 
% display refresh rate synchronization tests, so set to 1 to skip tests 
% and avoid errors, these tests are more important for tasks where timing
% needs to be very precise
Screen('Preference', 'SkipSyncTests', 1);

% check if all needed parameters are given:
if nargin < 6
    error('Must provide required input parameters "sID", "img_pair", "is_food_rewarded_first", "subject_code", and "taskVersion", and "block"!');
    Screen('CloseAll');
    ShowCursor;
    Priority(0);
    ListenChar(0);
end

% check whether arguments to revlRun() are correct
revlCheckArgs;

% select the pair of images to be displayed and whether the NJRE stimulus
% is rewarded in the first sequence of trials or not.
selectImgPair;


% Debugging: if turned on, task will not be displayed full screen
debug = 0; 
% debug = input('debug mode? (1 = yes, 0 = no): ');

% A.    Getting started
%--------------------------------------------------------------------------
revlSetPaths; 

% run and load prep file
prep = revlParams(taskVersion, true, blue_img, orange_img);

% Set stuff up (files, variables etc.), start PTB - Screen.
revlSetup

% Present instructions. Subject presses key to continue.
% revlInstr
WaitSecs(1);
drawfix;

Screen('Flip', wd);
if ~debug
    WaitSecs(2);
end

% A. Loop over all trials.
%--------------------------------------------------------------------------
aborted = false;
for t = 1:nt
    % C.2.1     Start of t: draw the stimuli according to t. This draws a
    % fixation cross with a stimulus on both sides:
    wd = revlDrawStim(wd, prep, 0, [], t, img);
    tm.stim(t) = Screen(wd, 'Flip');
    
    % Choice: wait until one has been selected or trial aborted, then draw
    % the stim again, but this time with the frame around the selected one
    nobreak = 0;
    while nobreak == 0
        [secs, keyCode, dSecs] = KbWait;
        a = find(keyCode);
        if length(a)==1;
            if(a == prep.left || a == prep.right || a == prep.abort)
                tm.choice(t)  = GetSecs;
                nobreak = 1;
                switch a
                    case prep.left
                        % find the location of stim 1. If it is on the
                        % left, you picked stim 1, otherwise you picked
                        % stim 2 
                        data.choice(t) = find(prep.locs(t,:) == 1);
                    case prep.right
                        data.choice(t) = find(prep.locs(t,:) == 2);
                end
            end
        end
    end
    
    if a == prep.abort
        abd = 'You have aborted the game';
        [wt] = Screen('TextBounds', wd, abd);
        xpos = round(wdw / 2 - wt(3) / 2);
        ypos = round(wdh / 2 - wt(4) / 2);
        Screen('Drawtext', wd, abd, xpos, ypos, prep.draw.white);
        Screen('flip', wd,[]);
        WaitSecs(1);
        % break out of trial-loop.
        break
    end
    
    % check if valid response (within boxes).
    % -----------------------------------------------------------------
    % determine feedback.
    data.outcome(t) = prep.feedback(t, data.choice(t));
    
    % present choice.
    wd = revlDrawStim(wd, prep, 1, data, t, img);
    Screen(wd, 'Flip');
    
    % Prepare the outcome.
    wd = revlDrawStim(wd, prep, 2, data, t, img);
    
    % Wait for cue-outcome interval time, then present the outcome.
    WaitSecs('UntilTime', tm.choice(t) + prep.time.coi);
    tm.outcome(t) = Screen(wd, 'Flip');
    
    % Make screen to black before switching to the next t. Record when
    % the screen flips to black. Note that the outcome is always onscreen
    % for the duration prep.time.iti, so that it's not shown longer for
    % the juice ts.
    drawfix;
    WaitSecs('UntilTime',tm.outcome(t) + prep.time.iti - prep.time.black);
    Screen(wd, 'Flip');

    % wait for start of new trial.
    WaitSecs('UntilTime', tm.outcome(t) + prep.time.iti);
    
end % end of trial loop.
data.RT = tm.choice - tm.stim;
data.totalReward = nansum(data.outcome);

% D   Save and wrap things up.
%--------------------------------------------------------------------------
Screen('FillRect', wd, prep.draw.black);
Screen(wd, 'Flip');
HideCursor;
WaitSecs(1);
data.sID = sID;
data.subject_code = subject_code;
data.today = today;
data.tm = tm;
data.prep = prep;
data.img_pair = img_pair;
data.is_food_rewarded_first = is_food_rewarded_first;
data.block = block;

% save data. If dataFile already exists, create unique dataFile
if exist(dataFile, 'file') % subject exists already
    randAttach = round(rand * 10000);
    dataFile = sprintf('%s_%04.0f.mat', dataFile(1:end - 4), randAttach);
end
save(dataFile,'data');

% give feedback 
text =  sprintf('Your total score is %d Euro. Your performance will be compared with the performance of the other participants.', data.totalReward);
[wt] = Screen('TextBounds',wd,text);
xpos = round(wdw / 2 - wt(3) / 2);
ypos = round(wdh / 2 - wt(4) / 2);
Screen('Drawtext', wd, text, xpos, ypos, prep.draw.white);
Screen('flip', wd,[]);
WaitSecs(2)

% Thank subjects
text =  'Thank you for participating';
[wt] = Screen('TextBounds', wd, text);
xpos = round(wdw / 2 - wt(3) / 2);
ypos = round(wdh / 2 - wt(4) / 2);
Screen('Drawtext', wd, text, xpos, ypos, prep.draw.white);
Screen('flip', wd,[]);

WaitSecs(1);
Screen('CloseAll');
clear Screen

% plot data %%% uncomment following line to show plot of data!
% revlPlotData(sID, dataPath);
end

