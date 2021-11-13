function [ output_args ] = timing()
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here



    
[wPtr, rect] = Screen('OpenWindow', 0);

% Set priority for script execution to realtime priority:
priorityLevel = MaxPriority(wPtr);
Priority(priorityLevel);

black = BlackIndex(wPtr); 
white = WhiteIndex(wPtr); 

FlipInterval = Screen('GetFlipInterval', wPtr);
slack = FlipInterval/2;

[VBLTimestamp StimulusOnsetTime] = Screen(wPtr, 'Flip', 0); 
p = 0.25; 
when = VBLTimestamp + p;
t1 = GetSecs;

for i=1:10
    Screen('FillRect', wPtr, black, [100 100 500 500]); 
    [VBLT StimulusOT] = Screen(wPtr, 'Flip', when-slack); 
    p = 0.25; 
    when=VBLT+p;
    Screen('FillRect', wPtr, white, [100 100 500 500]); 
    [VBLT StimulusOT] = Screen(wPtr, 'Flip', when-slack); 
    p = 0.25; 
    when = VBLTimestamp + p;
end

t2 = GetSecs;

Screen('Close', wPtr);
 
fprintf('Total time = %f flip interval = %f slack = %f\n', t2 - t1, ...
    FlipInterval, slack);

Priority(0);


end

