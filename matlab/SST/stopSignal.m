function stopSignal(subNo, duration, hand)
%
% Script for a stop-signal experiment.
%
% This demo shows a single image with a tone that is presented after a 
% predefined delay.  A keystroke deletes the images from the screen.
%
% Per lanciare lo script, digitare sulla console:
% stopSignal(24, 1.5, 1)
%
% Se si vuole lanciare lo script durante la spiegazione del compito al
% soggetto, usare 99 quale identificativo del soggetto:
% stopSignal(99, 1.5, 1)
% NOTA BENE: se si usa 99 per il numero del soggetto, il file precedente,
% che riporta 99 nel nome, verrà CANCELLATO!
% Quindi, quando si vogliono registrare i dati, usare un numero diverso da
% 99!
%
% Durante la fase di spiegazione delle istruzioni al soggetto, lanciare lo
% script con
% stopSignal(99, 1.5, 1)
% Per uscire, tenere premuto il tasto ESC.
% Se la tastiera non risponde, dopo essere usciti da Matlab, premere
% contemporaneamente i tasti ALT, CTRL, ESC.

% History:
%
% Corrado Caudek.



%--------------------------------------------------------------------------
% any preliminary stuff
%--------------------------------------------------------------------------

% Clear the workspace
clc;

% check for Opengl compatibility, abort otherwise
AssertOpenGL;

% neither macbook nor testing computer seem to interact properly with 
% display refresh rate synchronization tests, so set to 1 to skip tests 
% and avoid errors, these tests are more important for tasks where timing
% needs to be very precise
Screen('Preference', 'SkipSyncTests', 1);

%  Minimize extraneous warnings.
% oldLevel = Screen('Preference', 'Verbosity', 1);

% Check if all needed parameters given:
if nargin < 3
    error('Must provide required input parameters "subNo", "duration", and "hand"!');
end

% Reseed the random-number generator for each expt.
rand('state', sum(100 * clock));

% Make sure keyboard mapping is the same on all supported operating systems
% Apple MacOS/X, MS-Windows and GNU/Linux:
KbName('UnifyKeyNames');

% Query keycodes for ESCAPE key and Space key:
esc = KbName('ESCAPE');
space = KbName('space');

% Use input variable "hand" to determine response mapping for this session.
if (hand == 1)
    yesResp = KbName('c'); % "old" response via key 'c'
    noResp  = KbName('m'); % "new" response via key 'm'
else
    yesResp = KbName('m'); % Keys are switched in this case.
    noResp  = KbName('c');
end

% stops keystrokes from being printed to the matlab workspace
ListenChar(2);


%--------------------------------------------------------------------------
% file handling: define filenames of input files and result file
%--------------------------------------------------------------------------


% define filename of stimuli list based on user-specified order
templateFile = 'template.txt';

% define filename of data file to write to
datafilename = strcat('StopSignal_', num2str(subNo), '.txt'); 

% check for existing result file to prevent accidentally overwriting
% files from a previous subject/session (except for subject numbers > 99):
if subNo < 99 && fopen(datafilename, 'rt') ~= -1
    fclose('all');
    error('Result data file already exists! Choose a different subject number.');
else
    datafilepointer = fopen(datafilename, 'wt'); % open ASCII file for writing
end


% print column headings to data output file
fprintf(datafilepointer, '%s %s %s %s %s %s %s %s %s %s %s %s %s %s\n', ... 
    'subject', 'trial', 'unshuffledTrial', 'goStop', 'goNum', 'threat', ...
    'img', 'hand', 'SSD', 'DELTA', 'gotResponse', 'correctResp', 'resp', 'rt');


%--------------------------------------------------------------------------
% initialize sound routines: adapted from initializeSoundRoutines() in 
% PTB demos
%--------------------------------------------------------------------------

deviceid = -1;

% Request latency mode 2, which used to be the best one in our measurement:
reqlatencyclass = 2; % class 2 empirically the best, 3 & 4 == 2

% Requested output frequency, may need adaptation on some audio-hw:
freq = 96000;        % Must set this. 96khz, 48khz, 44.1khz.
buffersize = 64;     % Pointless to set this. Auto-selected to be optimal.

% Take hardware delay of MacBookPro into account: Assign it as bias.
% Needs to determined via measurement once for each piece of audio
% hardware:
latbias = (30 / freq)

% Initialize driver, request low-latency preinit:
InitializePsychSound(1);

% Open audio device for low-latency output:
pahandle = PsychPortAudio('Open', deviceid, [], reqlatencyclass, freq, 2, buffersize);

% Tell driver about hardwares inherent latency, determined via calibration
% once:
prelat = PsychPortAudio('LatencyBias', pahandle, latbias)
postlat = PsychPortAudio('LatencyBias', pahandle)

% Generate some beep sound 1000 Hz, 0.1 secs, 90% amplitude:
mynoise(1,:) = 0.9 * MakeBeep(1000, 0.1, freq);
mynoise(2,:) = mynoise(1,:);

% Fill buffer with data:
PsychPortAudio('FillBuffer', pahandle, mynoise);


%--------------------------------------------------------------------------
% Experiment.
% Embed core of code in try ... catch statement. If anything goes wrong
% inside the 'try' block (Matlab error), the 'catch' block is executed to
% clean up, save results, close the onscreen window etc.
%--------------------------------------------------------------------------


try

    % Get screenNumber of stimulation display. We choose the display with
    % the maximum index, which is usually the right one, e.g., the external
    % display on a Laptop:
    Nscreen = 0;
    screens = Screen('Screens');
    Nscreen = max(screens);
    
    % Hide the mouse cursor:
    HideCursor;
    
    % Returns as default the mean gray value of screen:
    gray = GrayIndex(Nscreen);

    % open a double buffered fullscreen window on the stimulation screen
    % 'screenNumber' and choose/draw a black background, 'windowPtr' is the handle
    % used to direct all drawing commands to that window - the "Name" of
    % the window, ~ is a place holder
    %[windowPtr, ~]=Screen('OpenWindow', screenNumber, gray);
    [windowPtr, rect] = Screen('OpenWindow', Nscreen, gray);
    [X,Y] = RectCenter(rect); 
    
    % Do dummy calls to GetSecs, WaitSecs, KbCheck to make sure
    % they are loaded and ready when we need them - without delays
    % in the wrong moment:
    KbCheck;
    WaitSecs(0.1);
    dummy = GetSecs;

    
    % Set priority for script execution to realtime priority:
    priorityLevel = MaxPriority(windowPtr);
    Priority(priorityLevel);
    
    

    % read list of conditions/stimuli
    % StimNumber  arbitrary number of stimulus
    % StimName    name of stimulus image
    % StimType    1=neutral, 2=aversive
    % CueType     1=passive, 2=space, 3=time, 4=objective
    % StartleTime 0=no startle, 1=early, 2=middle, 3=late
    
    
     % read list of conditions and stimulus parameters 
    [unshuffledTrial, goStop, goNum threat, img] = textread(templateFile, ...
        '%d %s %d %s %s');
    
    nTrials = length(unshuffledTrial);
    
    % randomize the elements of the vector 1:ntrials
    randomOrder = randperm(nTrials);
    
    
%--------------------------------------------------------------------------
% initialize variables used in the first trial of the trial loop
%--------------------------------------------------------------------------
    
    
    SSD = 0;
    DELTA = 0;
    errPrevStopTrial = 0;
    resp = 'NONE';
    rt = 0;
    

%--------------------------------------------------------------------------
% write message to subject
%--------------------------------------------------------------------------
    

    % Set text size (Most Screen functions must be called after
    % opening an onscreen window, as they only take window handles 
    % 'windowPtr' as input:
    Screen('TextSize', windowPtr, 32);
    

    % write message to subject
    str = sprintf('Press _%s_ for YES and _%s_ for NO\n\n', ...
        KbName(yesResp), KbName(noResp));
    message = ['\n' str '... press any key to begin ...'];
    DrawFormattedText(windowPtr, message, 'center', 'center', WhiteIndex(windowPtr));
    
    % Update the display to show the instruction text:
    Screen('Flip', windowPtr);
    % Wait for key press:
    KbWait;
    % Clear screen to background color:
    Screen('Flip', windowPtr);
    
   
%--------------------------------------------------------------------------
% loop through full list of trials
%--------------------------------------------------------------------------


    for trial = 1:nTrials
    
        % initialize KbCheck and variables to make sure they're
        % properly initialized/allocted by Matlab - this to avoid time
        % delays in the critical reaction time measurement part of the
        % script:        
        [keyIsDown, secs, keyCode] = KbCheck;
        % keyIsDown 1 if any key, including modifiers such as <shift>,
        % <control> or <caps lock> is down. 0 otherwise.
        % secs Time of keypress as returned by GetSecs.
        % A 256-element logical array.  Each bit
        % within the logical array represents one keyboard key. 
        % If a key is pressed, its bit is set, othewise the bit 
        % is clear. To convert a keyCode to a vector of key  
        % numbers use FIND(keyCode). To find a key's keyNumber 
        % use KbName or KbDemo.
        
        if (keyIsDown == 1)
            % Abort requested?
            if keyCode(esc)
                % Break out of display loop:
                break;
            end;
        end
      
        % blank screen
        Screen('FillRect', windowPtr, [192, 192, 192])
        Screen('Flip', windowPtr);
        
        WaitSecs(0.50) % interstimulus interval (1 s)

        % draw fixation crosss
        FixCross = [X-1, Y-40, X+1, Y+40; X-40, Y-1,X+40, Y+1];
        Screen('FillRect', windowPtr, [192, 192, 192])
        Screen('FillRect', windowPtr, [255, 255, 255], FixCross');
        Screen('Flip', windowPtr);
        WaitSecs(1.00)

        
        % generate a string like 'stim/img1.jpg';
        stimfilename = strcat('stim/', char(img(randomOrder(trial))), '.jpg');
        %stimfilename=strcat('stimuli/',char(StimName(trial)));

        % read stimulus image into matlab matrices 'I':
        I = imread(char(stimfilename));
        
        % make texture image out of image matrix 'I'
        txtIgm = Screen('MakeTexture', windowPtr, I);

        % Draw texture image to backbuffer. It will be automatically
        % centered in the middle of the display if you don't specify a
        % different destination:
        Screen('DrawTexture', windowPtr, txtIgm);

        % duration = 1.5; % per i soggetti che non riescono a fare il compito,
        % il parametro 'duration' può essere allungato.
        
        % Show stimulus on screen at next possible display refresh cycle,
        % and record stimulus onset time in 'StimulusOnsetTime':
        [VBLTimeStamp StimulusOnsetTime] = Screen('Flip', windowPtr);
        
         % increase DELTA depending on errors on STOP trials:
         if (errPrevStopTrial == 1 && goNum(randomOrder(trial)) == 0)
             if (SSD == 0) 
                 DELTA = -.200;
             else
                DELTA = DELTA - 0.0333;
             end
         elseif (errPrevStopTrial == 0 && goNum(randomOrder(trial)) == 0) 
             if (DELTA >= duration)
                 DELTA = duration;
             else
                DELTA = DELTA + 0.0333;
             end
         end
        
         % add tone for STOP trials
        if (goNum(randomOrder(trial)) == 0)
            SSD = .200 + DELTA;
            triggerTime = StimulusOnsetTime + SSD;
            % Start audio schedule, with times relative to 'triggerTime':
            PsychPortAudio('Start', pahandle, [], triggerTime);
        end

        gotResponse = 0;
        
        % while loop to show stimulus until subjects response or until
        % "duration" seconds elapsed.
        while (GetSecs - StimulusOnsetTime) <= duration
            % subjects can response before stimulus terminates
            if ( keyCode(yesResp) == 1 || keyCode(noResp) == 1 )
                gotResponse = 1;
                break;
            end
            % Check the keyboard for subjects response
            [keyIsDown, secs, keyCode] = KbCheck;
            % Wait 1 ms before checking the keyboard again to prevent
            % overload of the machine at elevated Priority():
            WaitSecs(0.001);
        end
        
        % Clear screen to background color after fixed 'duration'
        % or after subjects response (on test phase)
        Screen('Flip', windowPtr);
        
        % compute response time
        rt = round(1000 * (secs - StimulusOnsetTime));
        % get key pressed by subject
        resp = KbName(keyCode); 
        
        
        if (hand == 1)
            if (strcmp(resp, 'c'))
                response = 'yes';
            elseif (strcmp(resp, 'm'))
                response = 'no';
            else
                response = 'NORESP';
            end
        else
            if (strcmp(resp, 'c'))
                response = 'no';
            elseif (strcmp(resp, 'm'))
                response = 'yes';
            else
                response = 'NORESP';
            end            
        end
        
        
        % Check the keyboard for subjects response:
        [keyIsDown, secs, keyCode] = KbCheck;
        
        if (keyIsDown == 1)
            % Abort requested?
            if keyCode(esc)
                % Break out of display loop:
                break;
            end;
        end

        %clear the keyboard buffer
        while KbCheck; end
        
        correctResp = 9;
        if (    goNum(randomOrder(trial)) == 1 && gotResponse == 1) 
            correctResp = 1;
        elseif (goNum(randomOrder(trial)) == 1 && gotResponse == 0)
            correctResp = 0;
        elseif (goNum(randomOrder(trial)) == 0 && gotResponse == 1) 
            correctResp = 0;
        elseif (goNum(randomOrder(trial)) == 0 && gotResponse == 0)
            correctResp = 1;
        else
            correctResp = 9;
        end
        
        if (    goNum(randomOrder(trial)) == 0 && correctResp == 0)
            errPrevStopTrial = 1;
        elseif (goNum(randomOrder(trial)) == 0 && correctResp == 1)
            errPrevStopTrial = 0;
        end
            

        % Write trial result to file:
        fprintf(datafilepointer, '%i %i %i %s %i %s %s %i %f %f %i %i %s %i\n', ...
            subNo, ...
            trial, ...
            unshuffledTrial(randomOrder(trial)), ...
            char(goStop(randomOrder(trial))), ...
            goNum(randomOrder(trial)), ...
            char(threat(randomOrder(trial))), ...
            char(img(randomOrder(trial))), ...
            hand, ...
            SSD, ...
            DELTA, ...
            gotResponse, ...
            correctResp, ...
            response, ...
            rt);
        

        
        % reset response variables to 0 so a non-response on the next trial
        % won't be recorded as the response from this trial
        resp = '0';
        rt = 0;
        
        % clear from memory all textures/screens not currently displayed
        % these can accumulate over trials and cause memory/performance
        % problems and potentially a fatal error
        Screen('Close');
        
        
        % Wait for subject to release keys. 
        % You'll typically use this function to make sure that all keys are idle
        % before you start some new trial that collects keyboard responses, after
        % you've used KbCheck, KbWait or KbPressWait for collecting a response.
        KbReleaseWait;        
        
        
        
%         %Every 20th trial, tell observer to relax and wait
%         if(mod(trial, 4) == 0)
%             %Draw the wait text, roughly centered
%             str = sprintf('%d trials done. Press down arrow to continue', trial);
%             DrawFormattedText(windowPtr, str, 'center', 'center', WhiteIndex(windowPtr));
%             Screen('Flip', windowPtr);
%             
%             while 1
%                 %Wait 'till there is no key down
%                 while ~KbCheck
%                 end
%                 [a b c] = KbCheck;
%                 if c(KbName('DownArrow'))
%                     break;
%                 end
%             end
%             WaitSecs(1.0);
%         end
        

    end; % Trial done. Next trial...

    
%--------------------------------------------------------------------------
% cleanup
%--------------------------------------------------------------------------

    
    % close window, show mouse cursor, close data file, 
    % switch matlab back to priority 0 (normal priority), restore keyboard 
    % output to matlab window
    Screen('CloseAll');
    ShowCursor;
    fclose('all');
    Priority(0);
    ListenChar(0);
    % Screen('Preference', 'Verbosity', oldLevel);
    
    % End of session, close down driver:
    PsychPortAudio('Close', pahandle);
    
    fprintf('Done. Bye!\n');
    % end the experiment
    return;


%--------------------------------------------------------------------------
% 'try' section due to programming error etc.
%--------------------------------------------------------------------------


catch
    
    % do same cleanup as above
    Screen('CloseAll');
    ShowCursor;
    fclose('all');
    Priority(0);
    ListenChar(0);
    
    % output the error message that describes the error
    psychrethrow(psychlasterror);
    
    % end the experiment
    return;

end

