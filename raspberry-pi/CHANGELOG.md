# Raspberry Pi Support Changelog

## Multi-Model Support Update

This update extends the repository to support Raspberry Pi 3, 4, and 5 models in addition to the original Pi 4 support.

### ✅ Completed Changes

#### 1. Documentation Updates

- **Main README.md**: Updated to reflect Pi 3/4/5 support instead of just Pi 4
- **raspberry-pi/README.md**: Comprehensive updates with model-specific information
- **raspberry-pi/install/README.md**: Updated to mention all three models

#### 2. Configuration Templates

- **config-pi3.txt**: Optimized for Raspberry Pi 3 (BCM2837, 1GB RAM)
- **config-pi4.txt**: Optimized for Raspberry Pi 4 (BCM2711, 2GB/4GB/8GB RAM)  
- **config-pi5.txt**: Optimized for Raspberry Pi 5 (BCM2712, 4GB/8GB RAM)
- **setup-config.sh**: Automated script to detect Pi model and apply correct config

#### 3. Enhanced Documentation

- **Hardware specifications**: Detailed specs for all three models
- **Performance expectations**: Boot times, desktop capabilities, use cases
- **GPIO and expansion**: Information about connectors and capabilities
- **Model-specific troubleshooting**: Common issues and solutions for each model

#### 4. Installation Improvements

- **Model-agnostic firmware instructions**: Works for all Pi models
- **Template-based config setup**: Easy selection of correct configuration
- **Post-installation guidance**: How to update config after boot

### 🔧 Technical Details

#### Architecture Compatibility

All Raspberry Pi 3, 4, and 5 models use ARM64 (aarch64) architecture:

- **Pi 3**: BCM2837, Cortex-A53 @ 1.2GHz
- **Pi 4**: BCM2711, Cortex-A72 @ 1.8GHz  
- **Pi 5**: BCM2712, Cortex-A76 @ 2.4GHz

#### Key Differences

- **GPU memory allocation**: Pi 3 (64MB), Pi 4/5 (128MB)
- **PCIe support**: Pi 5 only (for NVMe expansion)
- **Power requirements**: Pi 3 (2.5A), Pi 4 (3A), Pi 5 (5A)
- **Performance**: Significant differences in boot time and desktop capability

#### Configuration Templates

Each model has optimized settings:

- **Pi 3**: Minimal settings, audio disabled to save memory
- **Pi 4**: HDMI hotplug, standard settings
- **Pi 5**: PCIe configuration, dual 4K support

### 🚀 Usage

#### For New Installations

1. Build image on Apple Silicon Mac (works for all models)
2. Choose appropriate config template during firmware setup
3. Flash to SD card and boot

#### For Existing Installations

1. Copy `setup-config.sh` to your Pi
2. Run `./setup-config.sh` to auto-detect and configure
3. Or specify model manually: `./setup-config.sh pi4`

### 📋 Files Added/Modified

#### New Files

- `raspberry-pi/postinstall/templates/config-pi3.txt`
- `raspberry-pi/postinstall/templates/config-pi4.txt`
- `raspberry-pi/postinstall/templates/config-pi5.txt`
- `raspberry-pi/postinstall/templates/setup-config.sh`

#### Modified Files

- `README.md` - Updated platform descriptions
- `raspberry-pi/README.md` - Comprehensive multi-model support
- `raspberry-pi/install/README.md` - Updated installation guidance

### 🎯 Benefits

1. **Unified approach**: Single build process works for all models
2. **Model optimization**: Each Pi gets appropriate configuration
3. **Easy maintenance**: Automated config detection and setup
4. **Better documentation**: Clear guidance for each model's capabilities
5. **Comprehensive troubleshooting**: Model-specific issue resolution

### 🔮 Future Enhancements

Potential improvements for future versions:

- Pre-built images with firmware included
- Automated firmware download script
- USB boot support
- Raspberry Pi-specific recipes (GPIO, camera)
- Performance tuning guides
