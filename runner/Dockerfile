FROM debian:bullseye-slim

# Metadata
LABEL maintainer="luc.dijon@clickpaddle.com"
LABEL org.opencontainers.image.source="https://github.com/clickpaddle/clickceler"

# Create a non-root user
RUN groupadd -g 1001 runner && \
    useradd -m -u 1001 -g runner runner

# Install dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        curl \
        unzip \
        jq \
        git \
        ca-certificates \
        libicu67 \
        libkrb5-3 \
        libssl1.1 \
        libcurl4 \
        libunwind8 \
        liblttng-ust0 \
	git \
        build-essential \
        ninja-build \
	cmake\
	zlib1g-dev \
        libedit-dev \
 	libpcre3-dev \
	libyaml-dev \
        default-jdk \
        python3-dev \
        libssl-dev \
        openssl \
        linux-libc-dev \
        texinfo \
        && rm -rf /var/lib/apt/lists/*


RUN     mkdir /home/runner/actions-runner &&\
	cd /home/runner/actions-runner && \
	git clone https://github.com/SWI-Prolog/swipl-devel.git && \
        cd swipl-devel && \
        git submodule update --init && \
        mkdir build &&\
        cd build &&\
        cmake -DCMAKE_INSTALL_PREFIX=/usr  -DINSTALL_DOCUMENTATION=OFF -DCMAKE_BUILD_TYPE=PGO -G Ninja .. &&\
        ninja &&\
        ctest -j $(nproc) --output-on-failure && \
        ninja install
	
# Set working directory
WORKDIR /home/runner/actions-runner

# Download GitHub runner (change version if needed)
ENV RUNNER_VERSION=2.317.0
RUN curl -o actions-runner.tar.gz -L https://github.com/actions/runner/releases/download/v${RUNNER_VERSION}/actions-runner-linux-x64-${RUNNER_VERSION}.tar.gz && \
    tar xzf actions-runner.tar.gz && \
    rm actions-runner.tar.gz

# Set permissions
RUN chown -R runner:runner /home/runner

# Switch to non-root user
USER runner

# Copy entrypoint script
COPY --chown=runner:runner entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Entrypoint
ENTRYPOINT ["/entrypoint.sh"]

#END