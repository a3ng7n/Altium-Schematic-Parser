#ifndef CLIP_H
#define CLIP_H

bool is_clipped(sample_device_t input_sample);
sample_device_t clip_long_to_device(sample_long_t input_sample, bool* clip_error);

#endif
