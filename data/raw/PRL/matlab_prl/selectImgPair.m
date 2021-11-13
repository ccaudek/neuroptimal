% select the pair of images to be displayed and whether the sad stimulus
% is rewarded in the first sequence of trials or not.
%
% Corrado Caudek 
% corrado.caudek@unifi.it
% 
% version 31-10-2017
%==========================================================================


% if is_food_rewarded_first == 1, then the sad image is rewarded in the
% first sequence of trials; if is_food_rewarded_first == 0, then the control
% image is rewarded in the first sequence of trials.

% the parameter img_pair is used to select which pair of images is 
% displayed (out of 15)
if (is_food_rewarded_first == 1) 
    
    if     (img_pair == 1)
        blue_img   = 'im_food_1.jpg'; % food image
        orange_img = 'im_cntr_1.jpg'; % control image
    elseif (img_pair == 2)
        blue_img   = 'im_food_2.jpg'; % food image
        orange_img = 'im_cntr_2.jpg'; % control image
    elseif (img_pair == 3)
        blue_img   = 'im_food_3.jpg'; % food image
        orange_img = 'im_cntr_3.jpg'; % control image    
    elseif (img_pair == 4)
        blue_img   = 'im_food_4.jpg'; % food image
        orange_img = 'im_cntr_4.jpg'; % control image   
    elseif (img_pair == 5)
        blue_img   = 'im_food_5.jpg'; % food image
        orange_img = 'im_cntr_5.jpg'; % control image   
    elseif (img_pair == 6)
        blue_img   = 'im_food_6.jpg'; % food image
        orange_img = 'im_cntr_6.jpg'; % control image  
    elseif (img_pair == 7)
        blue_img   = 'im_food_7.jpg'; % food image
        orange_img = 'im_cntr_7.jpg'; % control image  
    elseif (img_pair == 8)
        blue_img   = 'im_food_8.jpg'; % food image
        orange_img = 'im_cntr_8.jpg'; % control image  
    else
        message2 = sprintf('*** Input for img_pair must be a integer between 1 and 8 ***'); 
        msgbox(message2);
        return;
    end
    
elseif (is_food_rewarded_first == 0)
    
    if     (img_pair == 1)
        blue_img   = 'im_cntr_1.jpg'; % control image
        orange_img = 'im_food_1.jpg'; % food image
    elseif (img_pair == 2)
        blue_img   = 'im_cntr_2.jpg'; % control image
        orange_img = 'im_food_2.jpg'; % food image
    elseif (img_pair == 3)
        blue_img   = 'im_cntr_3.jpg'; % control image
        orange_img = 'im_food_3.jpg'; % food image  
    elseif (img_pair == 4)
        blue_img   = 'im_cntr_4.jpg'; % control image
        orange_img = 'im_food_4.jpg'; % food image  
    elseif (img_pair == 5)
        blue_img   = 'im_cntr_5.jpg'; % control image
        orange_img = 'im_food_5.jpg'; % food image  
    elseif (img_pair == 6)
        blue_img   = 'im_cntr_6.jpg'; % control image
        orange_img = 'im_food_6.jpg'; % food image 
    elseif (img_pair == 7)
        blue_img   = 'im_cntr_7.jpg'; % control image
        orange_img = 'im_food_7.jpg'; % food image 
    elseif (img_pair == 8)
        blue_img   = 'im_cntr_8.jpg'; % control image
        orange_img = 'im_food_8.jpg'; % food image 
    else
        message2 = sprintf('*** Input for img_pair must be a integer between 1 and 8 ***'); 
        msgbox(message2);
        return;
    end    
    
else
    message1 = sprintf('*** Input for is_food_rewarded_first must be either 0 (no) or 1 (yes) ***'); 
    msgbox(message1);
    return;
end


