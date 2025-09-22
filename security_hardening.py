# BEGIN: user added these matplotlib lines to ensure any plots do not pop-up in their UI
import matplotlib
matplotlib.use('Agg')  # Set the backend to non-interactive
import matplotlib.pyplot as plt
plt.ioff()
import os
os.environ['TERM'] = 'dumb'
# END: user added these matplotlib lines to ensure any plots do not pop-up in their UI
# filename: security_hardening.py
# execution: true
import os
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric import padding
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives import serialization

class BootSecurity:
    def __init__(self):
        # Generate or load keys
        self.private_key, self.public_key = self._load_or_generate_keys()
        
    def _load_or_generate_keys(self):
        """Load existing keys or generate new ones if not found"""
        try:
            # Try to load existing keys
            with open("boot_private_key.pem", "rb") as f:
                private_key = serialization.load_pem_private_key(
                    f.read(),
                    password=None,
                    backend=default_backend()
                )
            with open("boot_public_key.pem", "rb") as f:
                public_key = serialization.load_pem_public_key(
                    f.read(),
                    backend=default_backend()
                )
            return private_key, public_key
        except FileNotFoundError:
            # Generate new keys if not found
            private_key = rsa.generate_private_key(
                public_exponent=65537,
                key_size=4096,
                backend=default_backend()
            )
            public_key = private_key.public_key()
            
            # Save keys for future use
            with open("boot_private_key.pem", "wb") as f:
                f.write(private_key.private_bytes(
                    encoding=serialization.Encoding.PEM,
                    format=serialization.PrivateFormat.PKCS8,
                    encryption_algorithm=serialization.NoEncryption()
                ))
            with open("boot_public_key.pem", "wb") as f:
                f.write(public_key.public_bytes(
                    encoding=serialization.Encoding.PEM,
                    format=serialization.PublicFormat.SubjectPublicKeyInfo
                ))
            return private_key, public_key
    
    def sign_bootloader(self, bootloader_path):
        """Sign the bootloader file"""
        with open(bootloader_path, "rb") as f:
            bootloader_data = f.read()
        
        signature = self.private_key.sign(
            bootloader_data,
            padding.PSS(
                mgf=padding.MGF1(hashes.SHA256()),
                salt_length=padding.PSS.MAX_LENGTH
            ),
            hashes.SHA256()
        )
        
        with open(f"{bootloader_path}.sig", "wb") as f:
            f.write(signature)
    
    def verify_boot_signature(self, bootloader_path):
        """Verify bootloader signature"""
        try:
            with open(bootloader_path, "rb") as f:
                bootloader_data = f.read()
            with open(f"{bootloader_path}.sig", "rb") as f:
                signature = f.read()
            
            self.public_key.verify(
                signature,
                bootloader_data,
                padding.PSS(
                    mgf=padding.MGF1(hashes.SHA256()),
                    salt_length=padding.PSS.MAX_LENGTH
                ),
                hashes.SHA256()
            )
            return True
        except Exception as e:
            print(f"Signature verification failed: {str(e)}")
            return False

# Example usage
if __name__ == "__main__":
    security = BootSecurity()
    
    # Sign a test bootloader file (create if doesn't exist)
    bootloader_path = "/boot/loader.bin"
    if not os.path.exists(bootloader_path):
        with open(bootloader_path, "wb") as f:
            f.write(os.urandom(1024))  # Random test data
    
    security.sign_bootloader(bootloader_path)
    print(f"Bootloader signed and verification result: {security.verify_boot_signature(bootloader_path)}")
