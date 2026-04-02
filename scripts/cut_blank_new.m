function cut_blank(folderPath)
% cut_blank('/Users/willchang/Dropbox/Research/PhD study/Research/research projects/Greg/F0 pertubation and Mandarin tones/pilot data/pilot data 2024May/pilot002/actual_exp_rec/', '/Users/willchang/Dropbox/Research/PhD study/Research/research projects/Greg/F0 pertubation and Mandarin tones/pilot data/pilot data 2024May/pilot002/actual_exp_rec/cut_blank/')
    fileList = dir(fullfile(folderPath, '*.wav'));
    
    % Create output path inside input folder
    cutPath = fullfile(folderPath, 'cut_blank');
    if ~exist(cutPath, 'dir')
        mkdir(cutPath);
        disp(['Created folder: ', cutPath]);
    end

    for i = 1:length(fileList)
        filePath = fullfile(folderPath, fileList(i).name);
        [audioData, sampleRate] = audioread(filePath);
        
        % Find indices where absolute values are greater than the threshold
        above_threshold_indices = abs(audioData) > 0.001;

        % Find the index of the first and last element that exceeds the threshold
        start_index = find(above_threshold_indices, 1, 'first');
        end_index = find(above_threshold_indices, 1, 'last');

        % Check if both start and end indices are found
        if ~isempty(start_index) && ~isempty(end_index)
            % Extract the above-threshold portion of the vector
            audioData = audioData(start_index:end_index);

            % Save preprocessed audio with new name
            [~, baseFileName, ~] = fileparts(fileList(i).name);
            newAudioFileName = sprintf('cutblank-%s.wav', baseFileName);
            newAudioFilePath = fullfile(cutPath, newAudioFileName);
            audiowrite(newAudioFilePath, audioData, sampleRate);
        else
            disp(['Skipping file ', fileList(i).name, ' as no valid audio data found.']);
        end
    end
end