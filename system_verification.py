
import matplotlib
matplotlib.use('Agg') 
import matplotlib.pyplot as plt
plt.ioff()
import os
os.environ['TERM'] = 'dumb'

import os
import json
from security_hardening import BootSecurity
from hardware_integration import HardwareDetector
from menu_system import MenuSystem
from cross_language_auth import AuthStandardizer
def verify_system():
"""Verify all system components work together"""
print("=== System Verification ===")

security = BootSecurity()
bootloader_path = "loader.bin"
if not os.path.exists(bootloader_path):
with open(bootloader_path, "wb") as f:
f.write(b"TEST_BOOTLOADER_DATA")
security.sign_bootloader(bootloader_path)
print(f"Security: {security.verify_boot_signature(bootloader_path)}")

hardware = HardwareDetector()
hw_info = hardware.get_hardware_info()
print(f"Hardware: CPU {hw_info['cpu']['cores']} cores, {hw_info['memory']['total_gb']}GB RAM")

menu = MenuSystem()
menu.build_menu()
print(f"Menu: {len(menu.root_menu.children)} top-level items")

auth = AuthStandardizer()
password = "SecurePassword123!"
print(f"Auth: Valid password: {auth.validate_password(password)}")
print(f"Auth: Invalid password: {not auth.validate_password('weak')}")
if __name__ == "__main__":
verify_system()
