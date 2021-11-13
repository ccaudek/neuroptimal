% check input parameters
%
% check if the value of img_pair is allowed:
if ~(img_pair > 0 & img_pair < 9)
    error('The input parameters "img_pair" must have values in the range 1:8!');
    Screen('CloseAll');
    ShowCursor;
    Priority(0);
    ListenChar(0);
end

% check if the value of is_food_rewarded_first is allowed:
if ~(is_food_rewarded_first == 0 || is_food_rewarded_first == 1)
    error('The input parameters "is_food_rewarded_first" must have values 0 or 1!');
    Screen('CloseAll');
    ShowCursor;
    Priority(0);
    ListenChar(0);
end

% check if the value of taskVersion is allowed:
if ~(taskVersion == 1 || taskVersion == 2)
    error('The input parameters "taskVersion" must have values 1 or 2!');
    Screen('CloseAll');
    ShowCursor;
    Priority(0);
    ListenChar(0);
end

% check if the value of block is allowed:
if ~(block > 0 & block < 9)
    error('The input parameters "block" must be in the range 1:8!');
    Screen('CloseAll');
    ShowCursor;
    Priority(0);
    ListenChar(0);
end
